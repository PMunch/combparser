## This module is a slightly edited version of an original by kmizu:
## https://gist.github.com/kmizu/2b10c2bf0ab3eafecc1a825b892482f3
## The idea is to make this into a more user friendly library for
## creating parsers in Nim.
import strutils
import sequtils
import lists
import re

type
  Parser*[T] = proc(input: string): Maybe[(T, string)]
  Maybe*[T] = object
    value: T
    hasValue: bool
    errors: seq[string]

proc Just*[T](value: T): Maybe[T] =
  result.hasValue = true
  result.value = value
  result.errors = nil

proc Nothing*[T]: Maybe[T] =
  result.hasValue = false
  result.errors = nil

#proc Error*[T](error: string): Maybe[T] =
#  result.hasValue = false
#  result.errors = @[error]

proc addError*[T](old: Maybe[T], error: string): Maybe[T] =
  echo "Adding error \"" & error & "\" to: " & $old
  result = old
  if result.errors == nil:
    result.errors = @[error]
  else:
    result.errors = result.errors & error

proc Error*[T](error: string): Maybe[T] =
  addError[T](Nothing[T](), error)

proc regex*(regexStr: string): Parser[string] =
  ## Returns a parser that returns the string matched by the regex
  (proc (input: string): Maybe[(string, string)] =
    let (first, last) = findBounds(input, re(regexStr))
    echo "Match for regex " & regexStr & " in string \"" & input & "\" is (" & $first & ", " & $last & ")"
    if first == 0:
      Just((input[0 .. last], input[(last + 1) .. input.len]))
    else:
      Error[(string, string)]("String did not match regex " & regexStr &
        " got: " & $input)
  )

proc s*(value: string): Parser[string] =
  ## Start with parser. Returns a parser that matches if the input starts
  ## with the given string.
  (proc (input: string): Maybe[(string, string)] =
    if input.startsWith(value):
      echo "String " & input & " starts with \"" & value & "\""
      Just ((input[0 .. (value.len - 1)], input[value.len .. input.len]))
    else:
      echo "String " & input & " doesn't start with \"" & value & "\""
      Error[(string, string)]("String did not start with \"" & value & "\", got" &
        " " & input)
  )

proc any*(): Parser[string] =
  (proc (input: string): Maybe[(string, string)] =
    if input.len > 0:
      # echo $input[0]
      # echo $input[1..^1]
      Just(($input[0], input[1..^1]))
    else:
      Nothing[(string, string)]()
  )

proc absent*[T](body: Parser[T]): Parser[string] =
  (proc (input: string): Maybe[(string, string)] =
    let res = body(input)
    if res.hasValue:
      Error[(string, string)]("Expected absence of symbol but got " & input)
    else:
      # Just(("", input))
      Nothing[(string, string)]()
  )

proc reduce*[T](body: Parser[DoublyLinkedList[T]], op: proc(v1: T, v2: T): T): Parser[T] =
  (proc (input: string): Maybe[(T, string)] =
    let res = body(input)
    result = res[0]
    for i in res[1..^1]:
      result = op(result, i)
  )


proc repeat*[T](body: Parser[T]): Parser[DoublyLinkedList[T]] =
  ## Returns a parser that returns a linked list of the input parsers type.
  ## Used to accept more multiple elements matching a pattern. If there is
  ## no match this will return an empty list and all the input as it's rest
  (proc (input: string): Maybe[(DoublyLinkedList[T], string)] =
    var list = initDoublyLinkedList[T]()
    var rest = input
    while true:
      let xresult = body(rest)
      if xresult.hasValue:
        let (xvalue, xnext) = xresult.value
        list.append(xvalue)
        rest = xnext
      else:
        if xresult.errors != nil:
          return Just((list,rest)).addError("Error occured while parsing list")
        else:
          return Just((list, rest))
    nil
  )

proc `/`*[T](lhs, rhs: Parser[T]): Parser[T] =
  ## Or operation. Takes two parser and returns a parser that will return
  ## the first matching parser.
  (proc (input: string): Maybe[(T, string)] =
    let lresult = lhs(input)
    if lresult.hasValue:
      result = lresult
    else:
      let rresult = rhs(input)
      if rresult.errors != nil:
        if rresult.hasValue:
          result = addError[(T, string)](rresult, "Expected one of two values but found none")
        else:
          result = Error[(T, string)]("Expected one of two values but found none")
      else:
        result = rresult
      if lresult.errors != nil:
        result.errors = result.errors.concat(lresult.errors)
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
        result = Just(((lvalue, rvalue), rnext))
        if rresult.errors != nil:
          result = result.addError("Expected two values, but second value didn't match")
      else:
        if rresult.errors != nil:
          result = Nothing[((T, U), string)]()
          return result.addError("Expected two values, but second value didn't match")
        else:
          return Nothing[((T, U), string)]()
    else:
      if lresult.errors != nil:
        result = Nothing[((T, U), string)]()
        return result.addError("Expected two values, but second value didn't match")
      else:
        return Nothing[((T, U), string)]()
  )

proc silence*[T](parser: Parser[T]): Parser[T] =
  (proc (input: string): Maybe[(T, string)] =
    var xresult = parser(input)
    xresult.errors = nil
    echo "Silencing: " & $xresult
    return xresult
  )

proc map*[T, U](parser: Parser[T], f: (proc(value: T): U)): Parser[U] =
  ## Takes a parser and a function to converts it's type into another type and
  ## returns a parser that outputs the second type.
  (proc (input: string): Maybe[(U, string)] =
    let xresult = parser(input)
    echo "Mapping: " & $xresult.errors & ", " & $xresult.hasValue
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      result = Just((f(xvalue), xnext))
      if xresult.errors != nil:
        result.errors = xresult.errors
        return result.addError("Trying to map a function but got error in deeper match")
    else:
      if xresult.errors != nil:
        result = Nothing[(U, string)]()
        result.errors = xresult.errors
        return result.addError("Trying to map a function but got error in deeper match")
      else:
        return Nothing[(U, string)]()
  )

proc flatMap*[T, U](parser: Parser[T], f: (proc(value: T): Parser[U])): Parser[U] =
  ## Similar to map this takes a parser and a function to make a conversion. The difference
  ## is that while the above takes a converter from one type to another. This takes a converter
  ## from one type to a parser of another type.
  (proc (input: string): Maybe[(U, string)] =
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      (f(xvalue)(xnext)).addError("Trying to flatmap a function but got error in deeper match")
    else:
      if xresult.errors != nil:
        Error[(U, string)]("Trying to flatmap a function but got error in deeper match")
      else:
        Nothing[(U, string)]()
  )

proc chainl*[T](p: Parser[T], op: Parser[(proc(a: T, b: T): T)]): Parser[T] =
  ## Takes two parsers, one that returns a type, and a second that takes an operator over that
  ## type. Returns a new parser that parses zero or more occurences of the type separated by
  ## the operator and applies the operator to the types in a left-associative manner.
  (p + (op + p).repeat()).map(proc(values: (T, DoublyLinkedList[((proc(a: T, b: T): T), T)])): T =
    echo "------------------------------"
    let (x, xs) = values
    var a = x
    for fb in xs:
      let (f, b) = fb
      a = f(a, b)
    a)

when isMainModule:
  proc A(): Parser[int]

  proc M(): Parser[int]

  proc P(): Parser[int]

  proc number(): Parser[int]

  proc E(): Parser[int] = A()

  proc A(): Parser[int] = M().chainl(
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
      echo "Looking for expression"
      E().flatMap(proc(e: int): Parser[int] =
        regex(r"\s*\)\s*").map(proc(_: string): int =
          e))) / number()

  proc number(): Parser[int] = regex(r"\s*[0-9]+\s*").map(proc(n: string): int =
    parseInt(n.strip()))

  let t = E()("( 1 + 2 )  *   ( 3 + 4 )  Hello world")
  echo t.value
  for i, error in t.errors:
    echo " ".repeat(i) & error
  #[
  echo E()("1+2 * 5")

  echo regex(r"[0-9]*")("124ei51") #(value: (Field0: 124, Field1: ei51), hasValue: true)

  echo (s("\"") + (
    s("\"").absent() + any()
  ).repeat() + s("\""))("\"This is a test\"Wooo")
  ]#
