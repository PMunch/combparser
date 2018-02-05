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
  ErrorNodeKind = enum Branch, Leaf, Stem
  Error[T] = ref object
    case kind: ErrorNodeKind
      of Branch:
        left: Error
        right: Error
        branchError: string
      of Stem:
        stem: Error
        stemError: string
      of Leaf:
        leafError: string
    input: T
  Maybe*[T, U] = object
    value*: T
    hasValue*: bool
    errors*: Error[U]
  ParseError* = object of Exception

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
    result.errors = Error(kind: Leaf, leafError: error, input: input)
  else:
    result.errors = Error(kind: Stem, stem: old.errors, stemError: error, input: input)

proc Nothing*[T, U, V, W](left: Maybe[U, W], right: Maybe[V, W], error: string, input: W): Maybe[T, W] =
  result.hasValue = false
  if left.errors == nil and right.errors == nil:
    result.errors = Error(kind: Leaf, leafError: error, input: input)
  elif left.errors == nil:
    result.errors = Error(kind: Stem, stem: right.errors, stemError: error, input: input)
  elif right.errors == nil:
    result.errors = Error(kind: Stem, stem: left.errors, stemError: error, input: input)
  else:
    result.errors = Error(kind: Branch, left: left.errors, right: right.errors, branchError: error, input: input)


proc Nothing*[T, U, V](old: Maybe[U, V]): Maybe[T, V] =
  result.hasValue = false
  result.errors = old.errors

proc Nothing*[T, U](error: string, input: U): Maybe[T, U] =
  result.hasValue = false
  result.errors = Error(kind: Leaf, leafError: error, input: input)

proc Something*[T, U, V](ret: var Maybe[T, V], first: Maybe[U, V], error: string, input: V) =
  if first.errors == nil and ret.errors == nil:
    ret.errors = nil
  elif first.errors == nil:
    ret.errors = ret.errors
  elif ret.errors == nil:
    ret.errors = first.errors
  else:
    ret.errors = Error(kind: Branch, left: first.errors, right: ret.errors, branchError: error, input: input)

macro regex*(regexStr: string): Parser[string, string] =
  ## Returns a parser that returns the string matched by the regex
  let pos = lineInfo(callsite())
  result = quote do:
    (proc (input: string): Maybe[(string, string)] =
      let regex = re(`regexStr`)
      let (first, last) = findBounds(input, regex)
      if first == 0:
        Just((input[0 .. last], input[(last + 1) .. input.len]))
      else:
        Nothing[(string, string)](`pos` & ": Couldn't match regex \"" & `regexStr` & "\"", input)
    )

macro s*(value: string): StringParser[string] =
  ## Start with parser. Returns a parser that matches if the input starts
  ## with the given string.
  let pos = lineInfo(callsite())
  result = quote do:
    (proc (input: string): Maybe[(string, string)] =
      if input.startsWith(`value`):
        Just((input[0 .. (`value`.len - 1)], input[`value`.len .. input.len]))
      else:
        Nothing[(string, string)](`pos` & ": Starts with operation failed: input did not start with \"" & `value` & "\"", input)
    )

proc repeat*[T, U](body: Parser[T, U], atLeast: int = 1): Parser[DoublyLinkedList[T], U] =
  ## Returns a parser that returns a linked list of the input parsers type.
  ## Used to accept more multiple elements matching a pattern. If there is
  ## no match this will return an empty list and all the input as it's rest
  (proc (input: U): Maybe[(DoublyLinkedList[T], U)] =
    var
      list = initDoublyLinkedList[T]()
      rest = input
      count = 0
    if input.len == 0 and atLeast == 0:
      return Just((list, rest))
    while true:
      let xresult = body(rest)
      if xresult.hasValue:
        let (xvalue, xnext) = xresult.value
        list.append(xvalue)
        rest = xnext
        count += 1
      else:
        if rest == input:
          var ret: Maybe[(DoublyLinkedList[T], U)]
          if atLeast == 0:
            ret = Just[(DoublyLinkedList[T], U)](xresult, (list, rest))
          else:
            ret = Nothing[(DoublyLinkedList[T], U)](xresult, "Repeat found zero matching elements", rest)
          return ret
        else:
          if count >= atLeast:
            return Just[(DoublyLinkedList[T], U)](xresult, (list, rest))
          else:
            return Nothing[(DoublyLinkedList[T], U)]("Not enough elements matched. Expected at least " & $atLeast & " but got only " & $count, rest)
    nil
  )

proc `/`*[T, U](lhs, rhs: Parser[T, U]): Parser[T, U] =
  ## Or operation. Takes two parser and returns a parser that will return
  ## the first matching parser.
  (proc (input: U): Maybe[(T, U)] =
    let lresult = lhs(input)
    if lresult.hasValue:
      lresult
    else:
      let rresult = rhs(input)
      if rresult.hasValue:
        rresult
      else:
        Nothing[(T, U), (T, U)](lresult, rresult, "Either operation failed: Neither option matched", input)
  )

proc `+`*[T, U](lhs: Parser[T], rhs: Parser[U]): Parser[(T, U)] =
  ## And operation. Takes two parsers and returns a new parser with the tuple
  ## of the input parsers results. This only returns if both are true.
  (proc (input: string): Maybe[((T, U), string)] =
    let lresult = lhs(input)
    if lresult.hasValue:
      let (lvalue, lnext) = lresult.value
      let rresult = rhs(lnext)
      if rresult.hasValue:
        let (rvalue, rnext) = rresult.value
        var ret = Just(((lvalue, rvalue), rnext))
        #ret.errors = Error(kind: Branch, left: lresult.errors, right: rresult.errors, branchError: "Both operation succeded")
        #var ret = Nothing[((T, U), string)](lresult, rresult, "Both operation sucedded", input)
        #ret.hasValue = true
        #ret.value = ((lvalue, rvalue), rnext)
        return ret
      else:
        return Nothing[((T, U), string)](rresult, "Both operation failed: Unable to match second of two parsers", input)
    else:
      return Nothing[((T, U), string)](lresult, "Both operation failed: Unable to match first of two parsers", input)
  )

proc map*[T, U](parser: Parser[T], f: (proc(value: T): U)): Parser[U] =
  ## Takes a parser and a function to converts it's type into another type and
  ## returns a parser that outputs the second type.
  (proc (input: string): Maybe[(U, string)] =
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      return Just[(U, string)](xresult,(f(xvalue), xnext))
    else:
      return Nothing[(U, string)](xresult, "Unable to map onto bad output", input)
  )

proc flatMap*[T, U](parser: Parser[T], f: (proc(value: T): Parser[U])): Parser[U] =
  ## Similar to map this takes a parser and a function to make a conversion. The difference
  ## is that while the above takes a converter from one type to another. This takes a converter
  ## from one type to a parser of another type.
  (proc (input: string): Maybe[(U, string)] =
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

proc chainl*[T](p: Parser[T], q: Parser[(proc(a: T, b: T): T)], allowEmpty = true): Parser[T] =
  ## Takes two parsers, one that returns a type, and a second that takes an operator over that
  ## type. Returns a new parser that parses zero or more occurences of the type separated by
  ## the operator and applies the operator to the types in a left-associative manner.
  (proc(input: string): Maybe[(T, string)] =
    let
      first = p(input)
      (firstVal, rest) = first.value
    if not first.hasValue:
      return Nothing[(T, string), (T, string)](first, "Chainl operation failed: First value not matching", input)
    if rest.len == 0:
      return first
    var ret = (q + p).repeat(if allowEmpty: 0 else: 1).map(proc(values: DoublyLinkedList[((proc(a: T, b: T): T), T)]): T =
      let xs = values
      var a = firstVal
      for fb in xs:
        let (f, b) = fb
        a = f(a, b)
      a)(rest)
    Something(ret, first, "Chainl operation", input)
    return ret
  )

template chainl1*[T](p: Parser[T], q: Parser[(proc(a: T, b: T): T)]): Parser[T] =
  ## Same as the chainl operator but it requires at least one round of operations if there is
  ## sufficient amount of input for it.
  chainl(p, q, false)

proc getError*[T](input: Maybe[T], original: string = nil): string =
  result = ""
  if input.errors != nil:
    proc buildError(res: var string, level: int, node: Error) =
      case node.kind:
        of Leaf:
          if original != nil:
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
            res = res & "  ".repeat(level) & node.leafError & " on input \"" & node.input[0..<(node.input.find("\n"))] & "\"\n"
        of Stem:
          res = res & "  ".repeat(level) & node.stemError & " on input \"" & node.input[0..<(node.input.find("\n"))] & "\"\n"
          buildError(res, level + 1, node.stem)
        of Branch:
          res = res & "  ".repeat(level) & node.branchError & "\n"
          buildError(res, level + 1, node.left)
          buildError(res, level + 1, node.right)

    buildError(result, 0, input.errors)

proc `$`*[T](input: Maybe[T]): string =
  getError(input)

proc parse*[T](parser: Parser[T], input: string): T =
  let res = parser(input)
  if res.hasValue and (res.value[1] == "" or res.errors == nil):
    return res.value[0]
  else:
    raise newException(ParseError, "Unable to parse:\n" & getError(res, input).indent(2) & "\n")

when isMainModule:
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

  proc number(): Parser[int]

  proc Addition(): Parser[Node]

  proc Multiplication(): Parser[Node]

  proc Parenthesis(): Parser[Node]

  proc Expression(): Parser[Node] = Addition()

  proc Addition(): Parser[Node] = Multiplication().chainl(
    (s("+").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
    (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "+", left: lhs, right: rhs)))) /
    (s("-").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
    (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "-", left: lhs, right: rhs))))
  )

  proc Multiplication(): Parser[Node] = Parenthesis().chainl(
    (s("*").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
      (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "*", left: lhs, right: rhs)))) /
    (s("/").map(proc(_: string): (proc(lhs: Node, rhs: Node): Node) =
      (proc(lhs: Node, rhs: Node): Node = Node(kind: Operator, operator: "/", left: lhs, right: rhs))))
  )

  proc Parenthesis(): Parser[Node] =
    regex(r"\s*\(\s*").flatMap(proc(_: string): Parser[Node] =
      Expression().flatMap(proc(e: Node): Parser[Node] =
        regex(r"\s*\)\s*").map(proc(_: string): Node =
          e))) / number().map(proc(val: int): Node =
            Node(kind: Value, value: val))

  proc A(): Parser[int]

  proc M(): Parser[int]

  proc P(): Parser[int]

  proc E(): Parser[int] = A()

  proc A(): Parser[int] = M().chainl1(
    (s("+").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs + rhs))) /
    (s("-").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs - rhs)))
  )

  proc M(): Parser[int] = P().chainl(
    (s("*").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs * rhs))) /
    (s("/").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
      (proc(lhs: int, rhs: int): int = lhs div rhs)))
  )

  proc P(): Parser[int] =
    regex(r"\s*\(\s*").flatMap(proc(_: string): Parser[int] =
      E().flatMap(proc(e: int): Parser[int] =
        regex(r"\s*\)\s*").map(proc(_: string): int =
          e))) / number()

  proc number(): Parser[int] = regex(r"\s*[0-9]+\s*").map(proc(n: string): int =
    parseInt(n.strip()))

  echo parse(Expression(), "( 1 + 2 )  *   ( 3 + 4 )")
  echo "-----------------------------------------"
  echo parse(Expression(), " 1 + 2  *  3 + 4")
  echo "-----------------------------------------"
  #echo parse(Expression(), "1 + 2  *  3 + 4 Hello world")
  echo "-----------------------------------------"
  var res: Maybe[(int, string)]
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
  echo parse(E(), "1 + ")
