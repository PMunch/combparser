## This module is intended to be a user friendly library for
## creating parsers in Nim. Currently it only parses strings but
## the idea is to support mode kinds of input in the future.
## The initial effort focused on adding better error statements
## so that parsers are easier to debug.
##
## The starting point for this library was created by kmizu:
## https://gist.github.com/kmizu/2b10c2bf0ab3eafecc1a825b892482f3

import strutils
import lists
import re
import macros

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
  if not hasValue or rest != nil and (when rest is string: rest.len != 0 else: true):
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

macro nodeKind*(kind: NimNodeKind): untyped =
  let pos = lineInfo(callsite())
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

macro regex*(regexStr: string): untyped =# Parser[string, string] =
  ## Returns a parser that returns the string matched by the regex
  let pos = lineInfo(callsite())
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      let regex = re(`regexStr`)
      let (first, last) = findBounds(input, regex)
      Return(
        input = input,
        rest = if input.len == 0: input else: input[(last+1) .. input.high],
        hasValue = first == 0,
        value = if first == 0: input[0 .. last] else: nil,
        newerr = `pos` & ": Regex parser couldn't match " & (if first == 0: "more than " & $last & " characters" else: "any characters") & " on regex " & `regexStr`,
      )
    )

macro s*(value: string): untyped = # StringParser[string] =
  ## Start with parser. Returns a parser that matches if the input starts
  ## with the given string.
  let pos = lineInfo(callsite())
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      let hasValue = input.startsWith(`value`)
      Return(
        input = input,
        rest = if input.len == 0 or `value`.len > input.high: input else: input[`value`.len .. input.high],
        hasValue = hasValue,
        value = if hasValue: input[0 .. (`value`.len - 1)] else: nil,
        newerr = `pos` & ": Starts with parser couldn't match " & (if not input.startsWith(`value`): "as string didn't start with \"" & `value` & "\"" else: "full length of the string"),
      )
    )

macro charmatch*(charset: set[char]): untyped =
  ## Matches repeatedly against any character in the set
  let pos = lineInfo(callsite())
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
        value = if pos > 0: input[0 .. pos-1] else: nil,
        newerr = `pos` & ": Character set parser couldn't match " & (if pos > 0: "more than " & $pos & " characters" else: "any characters") & " with the charset " & repr(`charset`),
      )
    )

macro allbut*(but: string): untyped =
  ## Matches anything up to `but`.
  let pos = lineInfo(callsite())
  result = quote do:
    (proc (input: string): Maybe[(string, string), string] =
      var pos = input.find(`but`)
      if pos == -1:
        pos = input.len
      Return(
        input = input,
        rest = input[pos .. input.high],
        hasValue = pos > 0,
        value = if pos > 0: input[0 .. pos-1] else: nil,
        newerr = `pos` & ": All-but parser couldn't match " & (if pos > 0: "more than " & $pos & " characters" else: "any characters") & " stopping at " & `but`,
      )
    )

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
  ## Returns a parser that returns a linked list of the input parsers type.
  ## Used to accept more multiple elements matching a pattern. If there is
  ## no match this will return an empty list and all the input as it's rest
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
          value = if count >= atLeast: list else: nil,
          newerr = "Repeat operation failed: " & (if count < atLeast: "Not enough elements matched. Expected at least " & $atLeast & " but got only " & $count else: "Unable to match entire input"),
          lefterr = xresult.errors
        )
    nil
  )

proc `/`*[T, U](lhs, rhs: Parser[T, U]): Parser[T, U] =
  ## Or operation. Takes two parser and returns a parser that will return
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
  ## And operation. Takes two parsers and returns a new parser with the tuple
  ## of the input parsers results. This only returns if both are true.
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
      rest = if step == 2: rnext elif step == 1: lnext else: nil,
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
      rest = if xresult.hasValue: xresult.value[1] else: nil,
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

proc getError*[T](input: Maybe[T, string], original: string = nil, errorPath: seq[int] = nil): string =
  ## Will generate an error message from the given input. If original is supplied
  ## it will be used to show where in the input the error occured.
  result = ""
  if input.errors != nil:
    proc buildError(res: var string, level: int, node: Error[string], original: string, errorPath: seq[int]) =
      case node.kind:
        of Leaf:
          if original != nil and node.input != nil:
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
            if node.input == nil:
              res = res & "  ".repeat(level) & node.leafError & " on input nil\n"
            else:
              res = res & "  ".repeat(level) & node.leafError & " on input \"" & node.input[0..<(pos(node.input.find("\n"), node.input.len))] & "\"\n"
        of Stem:
          if node.input == nil:
            res = res & "  ".repeat(level) & node.stemError & " on input nil\n"
          else:
            res = res & "  ".repeat(level) & node.stemError & " on input \"" & node.input[0..<(pos(node.input.find("\n"), node.input.len))] & "\"\n"
          buildError(res, level + 1, node.stem, original, errorPath)
        of Branch:
          res = res & "  ".repeat(level) & node.branchError & "\n"
          if errorPath != nil:
            if errorPath[0] == 0:
              buildError(res, level + 1, node.left, original, errorPath[1..errorPath.high])
            else:
              buildError(res, level + 1, node.right, original, errorPath[1..errorPath.high])
          else:
            buildError(res, level + 1, node.left, original, nil)
            buildError(res, level + 1, node.right, original, nil)

    buildError(result, 0, input.errors, original, errorPath)
    result = result[0..result.high-1]

proc getShortError*[T](input: Maybe[T, string], original: string = nil): string =
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

macro raisehere*[T, U](parser: Parser[T, U], original: string = nil): untyped =
  ## For help with debugging or to achieve early termination. Will raise any
  ## error immediately if parser has no result, otherwise it does nothing.
  let pos = lineInfo(callsite())
  result = quote do:
    (proc (input: U): Maybe[(T, U), U] =
      result = parser(input)
      if not result.hasValue:
        raise newException(ParseError, `pos` & ": Unable to parse:\n" & getError(result, original).indent(2) & "\n")
    )

proc `$`*[T](input: Maybe[T, string]): string =
  if input.errors != nil:
    getError(input)
  else:
    $input.value

proc parse*[T](parser: Parser[T, string], input: string): T =
  let res = parser(input)
  if res.hasValue and (res.value[1] == "" or res.errors == nil):
    return res.value[0]
  else:
    raise newException(ParseError, "Unable to parse:\n" & getError(res, input).indent(2) & "\n")

when isMainModule:
  template runTest(name: string, parser: untyped, input: untyped, shouldError: bool): untyped =
    let r1 = parser(input)
    echo if r1.hasValue: name & " has value" else: name & " doesn't have a value"
    if r1.hasValue:
      echo r1.value[0]
      if r1.value[1].len == 0:
        echo "all consumed"
      else:
        echo "remaining characters: " & $r1.value[1].len
    echo if r1.errors == nil: name & " doesn't have errors" else: name & " has errors:\n" & r1.getError
    echo ""
    if shouldError == (r1.errors == nil):
      raise newException(AssertionError, "Expected parser to " & (if shouldError: "throw" else: "not throw") & " an error")

  runTest("Test 1", charmatch({'0'..'9'}), "123;", true)
  runTest("Test 2", charmatch({'0'..'9'}) + s(";"), "123;", false)
  runTest("Test 3", charmatch({'0'..'9'}) + s(";"), "123 ;", true)
  runTest("Test 4", s("hello"), "hello", false)
  runTest("Test 5", s("hello"), "hello world", true)
  runTest("Test 6", s("hello") / s("world"), "world", false)
  runTest("Test 7", s("hello") / s("world"), "worlds", true)
  runTest("Test 8", allbut("world"), "hello there", false)
  runTest("Test 9", allbut("world"), "hello world", true)
  runTest("Test 10", repeat(s("world")), "worldworldworld", false)
  runTest("Test 11", repeat(s("world")), "worldworldworldhello", true)
  runTest("Test 12", repeat(s("world"), atLeast = 4), "worldworldworld", true)
  runTest("Test 13", charmatch({'0'..'9'}).map(proc (input: string): int = parseInt(input)), "123", false)
  runTest("Test 14", charmatch({'0'..'9'}).map(proc (input: string): int = parseInt(input)), "123;", true)
  runTest("Test 15", charmatch({'0'..'9'}).map(proc (input: string): int = parseInt(input)) + s(";"), "123;", false)
  let x = (s("world") + ((s("world") + s("hello")) / (s("test") + s("hello"))))("worldworldworld")
  echo x.getError
  echo getShortError(x)

#when false:# isMainModule:
  type
    NodeKind = enum Operator, Value
    Node = ref object
      case kind: NodeKind
      of Value:
        value: int
      of Operator:
        operator: string
        left: Node
        right: Node

  proc `$`(tree: Node): string =
    case tree.kind:
      of Value:
        "Value(" & $tree.value & ")"
      of Operator:
        "Operator(" & $tree.left & " " & tree.operator & " " & $tree.right & ")"

  proc number(): Parser[int, string]

  proc Addition(): StringParser[Node]

  proc Multiplication(): StringParser[Node]

  proc Parenthesis(): StringParser[Node]

  proc Expression(): StringParser[Node] = Addition()

  proc Addition(): StringParser[Node] = Multiplication().chainl(
    (s("+").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
    (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "+", left: lhs, right: rhs)))) /
    (s("-").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
    (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "-", left: lhs, right: rhs))))
  )

  proc Multiplication(): StringParser[Node] = Parenthesis().chainl(
    (s("*").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
      (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "*", left: lhs, right: rhs)))) /
    (s("/").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
      (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "/", left: lhs, right: rhs))))
  )

  proc Parenthesis(): StringParser[Node] =
    regex(r"\s*\(\s*").flatMap(proc(_: string): StringParser[Node] =
      Expression().flatMap(proc(e: Node): StringParser[Node] =
        regex(r"\s*\)\s*").map(proc(_: string): Node =
          e))) / number().map(proc(val: int): Node =
            Node(kind: Value, value: val))

  proc A(): StringParser[int]

  proc M(): StringParser[int]

  proc P(): StringParser[int]

  proc E(): StringParser[int] = A()

  proc A(): StringParser[int] = M().chainl1(
    (s("+").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs + rhs))) /
    (s("-").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs - rhs)))
  )

  proc M(): StringParser[int] = P().chainl(
    (s("*").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs * rhs))) /
    (s("/").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs div rhs)))
  )

  proc P(): StringParser[int] =
    regex(r"\s*\(\s*").flatMap(proc(_: string): StringParser[int] =
      E().flatMap(proc(e: int): StringParser[int] =
        regex(r"\s*\)\s*").map(proc(_: string): int =
          e))) / number()

  proc number(): Parser[int, string] = regex(r"\s*[0-9]+\s*").map(proc(n: string): int =
    parseInt(n.strip()))

  echo parse(Expression(), "( 1 + 2 )  *   ( 3 + 4 )")
  echo "-----------------------------------------"
  echo parse(Expression(), " 1 + 2  *  3 + 4")
  echo "-----------------------------------------"
  #echo parse(Expression(), "1 + 2  *  3 + 4 Hello world")
  echo "-----------------------------------------"
  var res: Maybe[(int, string), string]
  res = E()("( 1 + 2 )  *   ( 3 + 4 )  Hello world")
  if res.hasValue:
    echo res
  else:
    echo res.getError
  echo "-----------------------------------------"
  res = E()("( 1 + 2 )  *   ( 3 + 4 )")
  if res.hasValue:
    echo res
  else:
    echo res.getError
  echo "-----------------------------------------"
  res = E()("( 1 + 2 ) \n * \n ( 3 + 4 ")
  if res.hasValue:
    echo res
  else:
    echo res.getError("( 1 + 2 ) \n * \n ( 3 + 4 ")
  echo "-----------------------------------------"
  res = E()("1 +")
  if res.hasValue:
    echo res
  else:
    echo res.getError
  echo "-----------------------------------------"
  echo parse(E(), "1 + 5")
  echo "-----------------------------------------"
  #echo parse(E(), "1 + ")
  macro testMacro(t: untyped): untyped =
    echo t.treeRepr
    let ret = (nodeKind(nnkStmtList) + nodeKind(nnkCommand).repeat())(@[t])
    if ret.hasValue:
      echo ret.value[0][0].treeRepr
      echo "List:"
      for i in ret.value[0][1]:
        echo i.treeRepr.indent(1, "  ")
      echo "Rest:"
      for i in ret.value[1]:
        echo i.treeRepr.indent(1, "  ")
    else:
      echo "Error!"
    return newStmtList()

  testMacro:
    echo "Hello"
    echo "world"
