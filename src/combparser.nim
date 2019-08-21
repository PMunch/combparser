## This library implements what is known as a parser combinator system. The idea
## behind this is to define your language by writing small, composable parsers
## and combining them into a larger logic. It was written mainly for the
## protobuf library which needed a parser that could run on compile-time to
## parse the protobuf specification. The original idea came from a small
## example library written by kmizu:
## https://gist.github.com/kmizu/2b10c2bf0ab3eafecc1a825b892482f3 but has been
## extensively rewritten with more matchers, more combinators, and proper error
## handling. It still has some way to go to be the user-friendly parser library
## it was indented to be, but it works well enough for now. For some examples
## on how to use the library see the examples folder. Most of the combinators
## have been written to accept not only string input and output, but any type.
## This means that this library could theoretically be used to parse not only
## string data but other types of data as well. Currently however the error
## reporting is only string based, but an alternate system is being considered.

import strutils
import re
import pegs
import macros

proc lineinfoStr(arg: tuple[filename: string, line: int, column: int]): string =
  arg.filename & "(" & $arg.line & ", " & $arg.column & ")"

type
  Parser*[T, U] = proc(input: U): Maybe[(T, U), U]
  StringParser*[T] = Parser[T, string]
  ErrorNodeKind* = enum Branch, Leaf, Stem
  Error*[T] = ref object
    case kind*: ErrorNodeKind
      of Branch:
        left*: Error[T]
        right*: Error[T]
        branchError*: string
      of Stem:
        stem*: Error[T]
        stemError*: string
      of Leaf:
        leafError*: string
    input*: T
  Maybe*[T, U] = object
    value*: T
    hasValue*: bool
    errors*: Error[U]
  ParseError* = object of Exception

proc Return*[T, W](input: W, rest: W, hasValue: bool, value: T, newerr: string, lefterr, righterr: Error[W] = nil): Maybe[(T, W), W] =
  result.hasValue = hasValue
  result.value = (value, rest)
  if not hasValue or rest.len != 0 and (when rest is string: rest.len != 0 else: true):
    if lefterr == nil and righterr == nil:
      result.errors = Error[W](kind: Leaf, leafError: newerr, input: input)
    elif lefterr == nil:
      result.errors = Error[W](kind: Stem, stem: righterr, stemError: newerr, input: input)
    elif righterr == nil:
      result.errors = Error[W](kind: Stem, stem: lefterr, stemError: newerr, input: input)
    else:
      result.errors = Error[W](kind: Branch, left: lefterr, right: righterr, branchError: newerr, input: input)
  else:
    result.errors = nil

proc Just*[T, U](value: T): Maybe[T, U] =
  result.hasValue = true
  result.value = value
  result.errors = nil

proc Just*[T, U, V](old: Maybe[U, V], value: T): Maybe[T, V] =
  result.hasValue = true
  result.value = value
  result.errors = old.errors

proc Nothing*[T, U, V](old: Maybe[U, V], error: string, input: V): Maybe[T, V] =
  result.hasValue = false
  if old.errors == nil:
    result.errors = Error[V](kind: Leaf, leafError: error, input: input)
  else:
    result.errors = Error[V](kind: Stem, stem: old.errors, stemError: error, input: input)

proc Nothing*[T, U, V, W](left: Maybe[U, W], right: Maybe[V, W], error: string, input: W): Maybe[T, W] =
  result.hasValue = false
  if left.errors == nil and right.errors == nil:
    result.errors = Error[W](kind: Leaf, leafError: error, input: input)
  elif left.errors == nil:
    result.errors = Error[W](kind: Stem, stem: right.errors, stemError: error, input: input)
  elif right.errors == nil:
    result.errors = Error[W](kind: Stem, stem: left.errors, stemError: error, input: input)
  else:
    result.errors = Error[W](kind: Branch, left: left.errors, right: right.errors, branchError: error, input: input)

proc Nothing*[T, U, V](old: Maybe[U, V]): Maybe[T, V] =
  result.hasValue = false
  result.errors = old.errors

proc Nothing*[T, U](error: string, input: U): Maybe[T, U] =
  result.hasValue = false
  result.errors = Error[U](kind: Leaf, leafError: error, input: input)

proc Something*[T, U, V](ret: var Maybe[T, V], first: Maybe[U, V], error: string, input: V) =
  if first.errors == nil and ret.errors == nil:
    ret.errors = nil
  elif first.errors == nil:
    ret.errors = ret.errors
  elif ret.errors == nil:
    ret.errors = first.errors
  else:
    ret.errors = Error[V](kind: Branch, left: first.errors, right: ret.errors, branchError: error, input: input)

macro nodeKindAux(kind: NimNodeKind, pos: string): untyped =
  result = quote do:
    (proc (input: seq[NimNode]): Maybe[(NimNode, seq[NimNode]), seq[NimNode]] =
      if input[0].kind == `kind`:
#input[0].sons.concat
        var
          rest = newSeq[NimNode](input[0].len)
          i = 0
        for child in input[0].children:
          rest[i] = child
          i += 1
        Just[(NimNode, seq[NimNode]), seq[NimNode]]((input[0], rest))
      else:
        Nothing[(NimNode, seq[NimNode]), seq[NimNode]](`pos` & ": Couldn't match node kind \"" & "" & "\"", input)
    )

template nodeKind*(kind: NimNodeKind): untyped =
  nodeKindAux(kind, lineinfoStr(instantiationInfo(fullpaths = true)))

macro regexAux(regexStr: string, pos: string): untyped =# Parser[string, string] =
  ## Returns a parser that returns the string matched by the regex
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      let regex = re(`regexStr`)
      let (first, last) = findBounds(input, regex)
      Return(
        input = input,
        rest = if input.len == 0: input else: input[(last+1) .. input.high],
        hasValue = first == 0,
        value = if first == 0: input[0 .. last] else: "",
        newerr = `pos` & ": Regex parser couldn't match " & (if first == 0: "more than " & $last & " characters" else: "any characters") & " on regex " & `regexStr`,
      )
    )

template regex*(regexStr: string): untyped =
  regexAux(regexStr, lineinfoStr(instantiationInfo(fullpaths = true)))

macro pegAux(pegStr: string, pos: string): untyped =# Parser[string, string] =
  ## Returns a parser that returns the string matched by the PEG
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      let peg = pegs.peg(`pegStr`)
      var matches = newSeq[string]()
      let (first, last) = findBounds(input, peg, matches)
      Return(
        input = input,
        rest = if input.len == 0: input else: input[(last+1) .. input.high],
        hasValue = first == 0,
        value = if first == 0: input[0 .. last] else: "",
        newerr = `pos` & ": PEG parser couldn't match " & (if first == 0: "more than " & $last & " characters" else: "any characters") & " on PEG " & `pegStr`,
      )
    )

template peg*(pegStr: string): untyped =# Parser[string, string] =
  pegAux(pegStr, lineinfoStr(instantiationInfo(fullpaths = true)))

macro sAux(value: string, pos: string): untyped = # StringParser[string] =
  ## Start with parser. Returns a parser that matches if the input starts
  ## with the given string.
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      let hasValue = input.startsWith(`value`)
      Return(
        input = input,
        rest = if input.len == 0 or `value`.len > input.len: input else: input[`value`.len .. input.high],
        hasValue = hasValue,
        value = if hasValue: input[0 .. (`value`.len - 1)] else: "",
        newerr = `pos` & ": Starts with parser couldn't match " & (if not input.startsWith(`value`): "as string didn't start with \"" & `value` & "\"" else: "full length of the string"),
      )
    )

template s*(value: string): untyped = # StringParser[string] =
  sAux(value, lineinfoStr(instantiationInfo(fullpaths = true)))

macro charmatchAux(charset: set[char], pos: string): untyped =
  ## Matches repeatedly against any character in the set
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      var pos = 0
      for c in input:
        if c in `charset`: pos += 1
        else: break
      Return(
        input = input,
        rest = input[pos .. input.high],
        hasValue = pos > 0,
        value = if pos > 0: input[0 .. pos-1] else: "",
        newerr = `pos` & ": Character set parser couldn't match " & (if pos > 0: "more than " & $pos & " characters" else: "any characters") & " with the charset " & repr(`charset`),
      )
    )

template charmatch*(charset: set[char]): untyped =
  charmatchAux(charset, lineinfoStr(instantiationInfo(fullpaths = true)))

macro allbutAux(but: string, pos: string): untyped =
  ## Matches anything up to ``but``.
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      var pos = input.find(`but`)
      if pos == -1:
        pos = input.len
      Return(
        input = input,
        rest = input[pos .. input.high],
        hasValue = pos > 0,
        value = if pos > 0: input[0 .. pos-1] else: "",
        newerr = `pos` & ": All-but parser couldn't match " & (if pos > 0: "more than " & $pos & " characters" else: "any characters") & " stopping at " & `but`,
      )
    )

template allbut*(but: string): untyped =
  allbutAux(but, lineinfoStr(instantiationInfo(fullpaths = true)))

proc optional*[T, U](parser: Parser[T, U]): Parser[T, U] =
  ## An optional wrapper, will remove any error from the parser and pretend it has
  ## a value if it doesn't already have one. This means that the value is left
  ## uninitialised and much be handled accordingly.
  (proc (input: U): Maybe[(T, U), U] =
    result = parser(input)
    if not result.hasValue:
      result.value[1] = input
    result.hasValue = true
    #result.errors = nil
  )

proc repeat*[T, U](body: Parser[T, U], atLeast: int = 1): Parser[seq[T], U] =
  ## Returns a parser that returns a sequence of the input parsers type.
  ## Used to accept more multiple elements matching a pattern. The ``atLeast``
  ## parameter can be used to set a lower limit on how many elements should be
  ## matched. If it is set to 0 and there is no match this will return an empty
  ## sequence and all the input as it's rest
  (proc (input: U): Maybe[(seq[T], U), U] =
    var
      list: seq[T] = @[]
      rest = input
      count = 0
    if input.len == 0 and atLeast == 0:
      return Just[(seq[T], U), U]((list, rest))
    while true:
      let xresult = body(rest)
      if xresult.hasValue:
        let (xvalue, xnext) = xresult.value
        list.add(xvalue)
        rest = xnext
        count += 1
      else:
        return Return(
          input = input,
          rest = rest,
          hasValue = count >= atLeast,
          value = if count >= atLeast: list else: @[],
          newerr = "Repeat operation failed: " & (if count < atLeast: "Not enough elements matched. Expected at least " & $atLeast & " but got only " & $count else: "Unable to match entire input"),
          lefterr = xresult.errors
        )
    nil
  )

proc `/`*[T, U](lhs, rhs: Parser[T, U]): Parser[T, U] =
  ## Or operation. Takes two parsers and returns a parser that will return
  ## the first matching parser.
  (proc (input: U): Maybe[(T, U), U] =
    let lresult = lhs(input)
    if lresult.hasValue:
      Return[T, U](
        input = input,
        rest = lresult.value[1],
        hasValue = true,
        value = lresult.value[0],
        newerr = "Either operation failed: didn't match entire input",
        lefterr = lresult.errors
      )
    else:
      let rresult = rhs(input)
      if rresult.hasValue:
        Return[T, U](
          input = input,
          rest = rresult.value[1],
          hasValue = true,
          value = rresult.value[0],
          newerr = "Either operation failed: didn't match entire input",
          lefterr = rresult.errors
        )
      else:
        var Nil: T
        Return[T, U](
          input = input,
          rest = input,
          hasValue = false,
          value = Nil,
          newerr = "Either operation failed: neither operation matched",
          lefterr = lresult.errors,
          righterr = rresult.errors
        )
  )

proc `+`*[T, U, V](lhs: Parser[T, V], rhs: Parser[U, V]): Parser[(T, U), V] =
  ## And operation. Takes two parsers and returns a new parser that returns a
  ## tuple of the input parsers results. This only returns if both parsers
  ## match succesfully.
  (proc (input: V): Maybe[((T, U), V), V] =
    var
      lvalue: T
      rvalue: U
      lnext, rnext: V
      lerrors, rerrors: Error[V]
      step = 0
    let lresult = lhs(input)
    lerrors = lresult.errors
    if lresult.hasValue:
      step = 1
      (lvalue, lnext) = lresult.value
      let rresult = rhs(lnext)
      rerrors = rresult.errors
      if rresult.hasValue:
        step = 2
        (rvalue, rnext) = rresult.value
    Return[(T, U), V](
      input = input,
      rest = if step == 2: rnext elif step == 1: lnext else: "",
      hasValue = step == 2,
      value = (lvalue, rvalue),
      newerr = "Both operation failed: " & (if step == 0: "Unable to match first of two parsers" elif step == 1: "Unable to match second of two parsers" else: "Unable to match all input"),
      lefterr = lerrors,
      righterr = rerrors
    )
  )

proc map*[T, U, V](parser: Parser[T, V], f: (proc(value: T): U)): Parser[U, V] =
  ## Takes a parser and a function to converts it's type into another type and
  ## returns a parser that outputs the second type.
  (proc (input: V): Maybe[(U, V), V] =
    let xresult = parser(input)
    var x: U
    Return[U, V](
      input = input,
      rest = if xresult.hasValue: xresult.value[1] else: "",
      hasValue = xresult.hasValue,
      value = if xresult.hasValue: f(xresult.value[0]) else: x,
      newerr = "Unable to map onto output with error",
      lefterr = xresult.errors
    )
  )

proc flatMap*[T, U, V](parser: Parser[T, V], f: (proc(value: T): Parser[U, V])): Parser[U, V] =
  ## Similar to map this takes a parser and a function to make a conversion. The difference
  ## is that while the above takes a converter from one type to another. This takes a converter
  ## from one type to a parser of another type.
  (proc (input: V): Maybe[(U, V), V] =
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      var ret = f(xvalue)(xnext)
      #if ret.errors == nil:
      #  ret.errors = xresult.errors
      Something(ret, xresult, "flatMap operation", input)
      return ret
    else:
      let ret = Nothing[(U, string)](xresult, "Unable to flat-map onto bad output", input)
      return ret
  )

proc chainl*[T, U](p: Parser[T, U], q: Parser[(proc(a: T, b: T): T), U], allowEmpty = true): Parser[T, U] =
  ## Takes two parsers, one that returns a type, and a second that takes an operator over that
  ## type. Returns a new parser that parses zero or more occurences of the type separated by
  ## the operator and applies the operator to the types in a left-associative manner.
  (proc(input: U): Maybe[(T, U), U] =
    let
      first = p(input)
      (firstVal, rest) = first.value
    if not first.hasValue:
      return Nothing[(T, U), (T, U)](first, "Chainl operation failed: First value not matching", input)
    if rest.len == 0:
      return first
    var ret = (q + p).repeat(if allowEmpty: 0 else: 1).map(proc(values: seq[((proc(a: T, b: T): T), T)]): T =
      let xs = values
      var a = firstVal
      for fb in xs:
        let (f, b) = fb
        a = f(a, b)
      a)(rest)
    Something(ret, first, "Chainl operation", input)
    return ret
  )

template chainl1*[T, U](p: Parser[T, U], q: Parser[(proc(a: T, b: T): T), U]): Parser[T, U] =
  ## Same as the chainl operator but it requires at least one round of operations if there is
  ## sufficient amount of input for it.
  chainl(p, q, false)

proc pos(first, second: int): int =
  if first >= 0:
    first
  else:
    second

proc ignorefirst*[T](first: StringParser[string], catch: StringParser[T]): StringParser[T] =
  ## Matches ``catch`` and ignores if it is preceded by ``first``. So both the
  ## input "Hello world" and "world" would match a parser
  ## ``ignorefirst(s("Hello "), s("world"))`` and return only "world".
  (first + catch).map(proc(input: tuple[f1: string, f2: T]): T = input.f2) / catch

proc ignorelast*[T](catch: StringParser[T], last: StringParser[string]): StringParser[T] =
  ## Same as ignorefirst, but ignores the last of the two arguments instead.
  (catch + last).map(proc(input: tuple[f1: T, f2: string]): T = input.f1) / catch

template ignoresides*(first, catch, last: typed): untyped =
  ## Wrapper that translates to ``ignorelast(ignorefirst(first, catch), last)``
  ignorefirst(first, catch).ignorelast(last)

proc andor*(first, last: StringParser[string]): StringParser[string] =
  ## Matches either ``first`` followed by ``last`` or just ``first`` or
  ## ``last``. In the case that bath are matched they will be concatenated.
  (first + last).map(proc(input: tuple[f1, f2: string]): string = input.f1 & input.f2) /
    (first / last)

proc getError*[T](input: Maybe[T, string], original: string = "", errorPath: seq[int] = @[]): string =
  ## Will generate an error message from the given input. If original is supplied
  ## it will be used to show where in the input the error occured.
  result = ""
  if input.errors != nil:
    proc buildError(res: var string, level: int, node: Error[string], original: string, errorPath: seq[int]) =
      case node.kind:
        of Leaf:
          if original.len != 0 and node.input.len != 0:
            let
              pos = original.rfind(node.input)
              startStr = original[0..<pos]
              startLine = startStr[startStr.rfind("\n")+1..<startStr.len]
              endStrPos = node.input.find("\n")
              endStr = node.input[0..<(if endStrPos > 0: endStrPos else: node.input.len)]
              newLine = "  ".repeat(level) & node.leafError & " on input \""
            res = res & newLine & startLine & endStr & "\"\n"
            res = res & " ".repeat(newLine.len + startLine.len) & "^\n"
          else:
            if node.input.len == 0:
              res = res & "  ".repeat(level) & node.leafError & " on input nil\n"
            else:
              res = res & "  ".repeat(level) & node.leafError & " on input \"" & node.input[0..<(pos(node.input.find("\n"), node.input.len))] & "\"\n"
        of Stem:
          if node.input.len == 0:
            res = res & "  ".repeat(level) & node.stemError & " on input nil\n"
          else:
            res = res & "  ".repeat(level) & node.stemError & " on input \"" & node.input[0..<(pos(node.input.find("\n"), node.input.len))] & "\"\n"
          buildError(res, level + 1, node.stem, original, errorPath)
        of Branch:
          res = res & "  ".repeat(level) & node.branchError & "\n"
          if errorPath.len != 0:
            if errorPath[0] == 0:
              buildError(res, level + 1, node.left, original, errorPath[1..errorPath.high])
            else:
              buildError(res, level + 1, node.right, original, errorPath[1..errorPath.high])
          else:
            buildError(res, level + 1, node.left, original, @[])
            buildError(res, level + 1, node.right, original, @[])

    buildError(result, 0, input.errors, original, errorPath)
    result = result[0..result.high-1]

proc getShortError*[T](input: Maybe[T, string], original: string = ""): string =
  result = ""
  if input.errors != nil:
    var
      shortestLen = int.high
      shortestPath: seq[int] = @[]
    proc searchShorter(node: Error[string], path: seq[int]) =
      case node.kind:
        of Leaf:
          if node.input.len < shortestLen:
            shortestLen = node.input.len
            shortestPath = path
        of Stem:
          searchShorter(node.stem, path)
        of Branch:
          searchShorter(node.left, path & 0)
          searchShorter(node.right, path & 1)
    searchShorter(input.errors, @[])
    return getError(input, original, shortestPath)

proc onerror*[T, U](parser: Parser[T, U], message: string, wrap = false): Parser[T, U] =
  ## Changes the error message of a parser. This way custom errors can be created for
  ## matchers. If the wrap flag is set to true, the message will be inserted as a
  ## parent of all underlying errors. Otherwise it will replace them.
  (proc (input: U): Maybe[(T, U), U] =
    result = parser(input)
    if not result.hasValue:
      if wrap == false:
        result.errors = Error[U](kind: Leaf, leafError: message, input: input)
      else:
        result.errors = Error[U](kind: Stem, stemError: message, stem: result.errors, input: input)
  )

macro raisehereAux[T, U](parser: Parser[T, U], original: string = "", pos: string): untyped =
  ## For help with debugging or to achieve early termination. Will raise any
  ## error immediately if parser has no result, otherwise it does nothing.
  result = quote do:
    (proc (input: U): Maybe[(T, U), U] =
      result = parser(input)
      if not result.hasValue:
        raise newException(ParseError, `pos` & ": Unable to parse:\n" & getError(result, original).indent(2) & "\n")
    )

template raisehere*[T, U](parser: Parser[T, U], original: string = ""): untyped =
  raisehereAux(parser, original, lineinfoStr(instantiationInfo(fullpaths = true)))

proc `$`*[T](input: Maybe[T, string]): string =
  if input.errors != nil:
    getError(input)
  else:
    $input.value

proc parse*[T](parser: Parser[T, string], input: string, longError = false): T =
  ## Convenience procedure to call a parser and check if there are any errors.
  ## Raises a ParseError exception with an attached error message. Should you
  ## want more information in the error message set longError to true.
  let res = parser(input)
  if res.hasValue and (res.value[1] == "" or res.errors == nil):
    return res.value[0]
  else:
    if longError:
      raise newException(ParseError, "Unable to parse:\n" & getError(res, input).indent(2) & "\n")
    else:
      raise newException(ParseError, "Unable to parse:\n" & getShortError(res, input).indent(2) & "\n")
