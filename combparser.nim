## This module is a slightly edited version of an original by kmizu:
## https://gist.github.com/kmizu/2b10c2bf0ab3eafecc1a825b892482f3
## The idea is to make this into a more user friendly library for
## creating parsers in Nim.
import strutils
import lists
import re
import termstyle

var debug = 0

type
  Parser*[T] = proc(input: string): Maybe[(T, string)]
  Maybe*[T] = object
    value: T
    hasValue: bool

proc Just*[T](value: T): Maybe[T] =
  result.hasValue = true
  result.value = value

proc Nothing*[T]: Maybe[T] =
  result.hasValue = false

proc regex*(regexStr: string): Parser[string] =
  let regex = re(regexStr)
  ## Returns a parser that returns the string matched by the regex
  (proc (input: string): Maybe[(string, string)] =
    echo red("Trying to match regex " & regexStr & " to " & input)
    let (first, last) = findBounds(input, regex)
    if first == 0:
      Just((input[0 .. last], input[(last + 1) .. input.len]))
    else:
      Nothing[(string, string)]()
  )

proc repeat*[T](body: Parser[T]): Parser[DoublyLinkedList[T]] =
  ## Returns a parser that returns a linked list of the input parsers type.
  ## Used to accept more multiple elements matching a pattern. If there is
  ## no match this will return an empty list and all the input as it's rest
  (proc (input: string): Maybe[(DoublyLinkedList[T], string)] =
    debug += 1
    let internalDebug = debug
    echo green($internalDebug & ": Trying to repeatedly match on " & input)
    var list = initDoublyLinkedList[T]()
    var rest = input
    while true:
      let xresult = body(rest)
      if xresult.hasValue:
        let (xvalue, xnext) = xresult.value
        list.append(xvalue)
        rest = xnext
      else:
        echo green($internalDebug & ": Done repeatedly matching on " & input)
        return Just((list, rest))
    nil
  )

proc `/`*[T](lhs, rhs: Parser[T]): Parser[T] =
  ## Or operation. Takes two parser and returns a parser that will return
  ## the first matching parser.
  (proc (input: string): Maybe[(T, string)] =
    debug += 1
    let internalDebug = debug
    echo blue($internalDebug & ": Checking if either of two parsers match on " & input)
    let lresult = lhs(input)
    if lresult.hasValue:
      echo blue($internalDebug & ": First option matched on " & input)
      lresult
    else:
      echo blue($internalDebug & ": Second option matched on " & input)
      rhs(input)
  )

proc `+`*[T, U](lhs: Parser[T], rhs: Parser[U]): Parser[(T, U)] =
  ## And operation. Takes two parsers and returns a new parser with the tuple
  ## of the input parsers results. This only returns if both are true.
  (proc (input: string): Maybe[((T, U), string)] =
    debug += 1
    let internalDebug = debug
    echo yellow($internalDebug & ": Checking if both of two parsers match on " & input)
    let lresult = lhs(input)
    if lresult.hasValue:
      let (lvalue, lnext) = lresult.value
      let rresult = rhs(lnext)
      if rresult.hasValue:
        let (rvalue, rnext) = rresult.value
        echo yellow($internalDebug & ": Succesfully matched both on " & input)
        Just (((lvalue, rvalue), rnext))
      else:
        echo yellow($internalDebug & ": Unable to match second on " & input)
        Nothing[((T, U), string)]()
    else:
      echo yellow($internalDebug & ": Unable to match first on " & input)
      Nothing[((T, U), string)]()
  )

proc s*(value: string): Parser[string] =
  ## Start with parser. Returns a parser that matches if the input starts
  ## with the given string.
  (proc (input: string): Maybe[(string, string)] =
    debug += 1
    let internalDebug = debug
    echo magenta($internalDebug & ": Checking if string \"" & input & "\" starts with " & value)
    if input.startsWith(value):
      echo magenta($internalDebug & ": String \"" & input & "\" started with " & value)
      Just ((input[0 .. (value.len - 1)], input[value.len .. input.len]))
    else:
      echo magenta($internalDebug & ": String \"" & input & "\" didn't start with " & value)
      Nothing[(string, string)]()
  )

proc map*[T, U](parser: Parser[T], f: (proc(value: T): U)): Parser[U] =
  ## Takes a parser and a function to converts it's type into another type and
  ## returns a parser that outputs the second type.
  (proc (input: string): Maybe[(U, string)] =
    debug += 1
    let internalDebug = debug
    echo cyan($internalDebug & ": Mapping a parser to another with input " & input)
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      echo cyan($internalDebug & ": Mapping succeded with input " & input)
      Just((f(xvalue), xnext))
    else:
      echo cyan($internalDebug & ": Mapping failed with input " & input)
      Nothing[(U, string)]()
  )

proc flatMap*[T, U](parser: Parser[T], f: (proc(value: T): Parser[U])): Parser[U] =
  ## Similar to map this takes a parser and a function to make a conversion. The difference
  ## is that while the above takes a converter from one type to another. This takes a converter
  ## from one type to a parser of another type.
  (proc (input: string): Maybe[(U, string)] =
    debug += 1
    let internalDebug = debug
    echo cyan($internalDebug & ": Flat-mapping a  parser to another with input " & input)
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      echo cyan($internalDebug & ": Flat-mapping succeded with input " & input)
      f(xvalue)(xnext)
    else:
      echo cyan($internalDebug & ": Flat-mapping failed with input " & input)
      Nothing[(U, string)]()
  )

proc chainl*[T](p: Parser[T], q: Parser[(proc(a: T, b: T): T)]): Parser[T] =
  (p + (q + p).repeat()).map(proc(values: (T, DoublyLinkedList[((proc(a: T, b: T): T), T)])): T =
    echo "Chained!"
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
      E().flatMap(proc(e: int): Parser[int] =
        regex(r"\s*\)\s*").map(proc(_: string): int =
          e))) / number()

  proc number(): Parser[int] = regex(r"\s*[0-9]*\s*").map(proc(n: string): int =
    parseInt(n.strip()))

  echo E()("( 1 + 2 )  *   ( 3 + 4 )  Hello world")
  #echo E()("1+2 * 5")

  #echo regex(r"[0-9]*")("124ei51") #(value: (Field0: 124, Field1: ei51), hasValue: true)

