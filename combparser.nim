## This module is a slightly edited version of an original by kmizu:
## https://gist.github.com/kmizu/2b10c2bf0ab3eafecc1a825b892482f3
## The idea is to make this into a more user friendly library for
## creating parsers in Nim.
import strutils
import lists
import re

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

proc regex*(regex: Regex): Parser[string] =
  (proc (input: string): Maybe[(string, string)] =
    let (first, last) = findBounds(input, regex)
    if first == 0:
      Just((input[0 .. last], input[(last + 1) .. input.len]))
    else:
      Nothing[(string, string)]()
  )

proc repeat*[T](body: Parser[T]): Parser[DoublyLinkedList[T]] =
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
        return Just((list, rest))
    nil
  )

proc `/`*[T](lhs, rhs: Parser[T]): Parser[T] = 
  (proc (input: string): Maybe[(T, string)] = 
    let lresult = lhs(input)
    if lresult.hasValue:
      lresult
    else:
      rhs(input)
  )

proc `+`*[T, U](lhs: Parser[T], rhs: Parser[U]): Parser[(T, U)] =
  (proc (input: string): Maybe[((T, U), string)] =
    let lresult = lhs(input)
    if lresult.hasValue:
      let (lvalue, lnext) = lresult.value
      let rresult = rhs(lnext)
      if rresult.hasValue:
        let (rvalue, rnext) = rresult.value
        Just (((lvalue, rvalue), rnext))
      else:
        Nothing[((T, U), string)]()
    else:
      Nothing[((T, U), string)]()
  )

proc s*(value: string): Parser[string] =
  (proc (input: string): Maybe[(string, string)] =
    if input.startsWith(value):
      Just ((input[0 .. (value.len - 1)], input[value.len .. input.len]))
    else:
      Nothing[(string, string)]()
  )

proc map*[T, U](parser: Parser[T], f: (proc(value: T): U)): Parser[U] =
  (proc (input: string): Maybe[(U, string)] = 
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      Just((f(xvalue), xnext))
    else:
      Nothing[(U, string)]()
  )

proc flatMap*[T, U](parser: Parser[T], f: (proc(value: T): Parser[U])): Parser[U] =
  (proc (input: string): Maybe[(U, string)] = 
    let xresult = parser(input)
    if xresult.hasValue:
      let (xvalue, xnext) = xresult.value
      f(xvalue)(xnext)
    else:
      Nothing[(U, string)]()
  )

proc chainl*[T](p: Parser[T], q: Parser[(proc(a: T, b: T): T)]): Parser[T] =
  (p + (q + p).repeat()).map(proc(values: (T, DoublyLinkedList[((proc(a: int, b: int): int), T)])): T =
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
    (s("+").map(proc(s: string): (proc(lhs: int, rhs: int): int) = 
      (proc(lhs: int, rhs: int): int = lhs + rhs))) / 
    (s("-").map(proc(_: string): (proc(lhs: int, rhs: int): int) = 
      (proc(lhs: int, rhs: int): int = lhs - rhs)))
  )

  proc M(): Parser[int] = P().chainl(
    (s("*").map(proc(s: string): (proc(lhs: int, rhs: int): int) = 
      (proc(lhs: int, rhs: int): int = lhs * rhs))) / 
    (s("/").map(proc(_: string): (proc(lhs: int, rhs: int): int) = 
      (proc(lhs: int, rhs: int): int = lhs div rhs)))
  )

  proc P(): Parser[int] =
    s("(").flatMap(proc(_: string): Parser[int] =
      E().flatMap(proc(e: int): Parser[int] =
        s(")").map(proc(_: string): int =
          e))) / number()

  proc number(): Parser[int] = regex(re"[0-9]+").map(proc(n: string): int = 
    parseInt(n))

  echo E()("(1+2)*(3+4)")
