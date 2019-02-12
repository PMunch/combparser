## This example shows how you can use combparser to parse strings and perform
## actions to reduce it down into a single result.
import "../src/combparser", strutils

proc Number(): Parser[int, string]

proc Addition(): StringParser[int]

proc Multiplication(): StringParser[int]

proc Parenthesis(): StringParser[int]

proc Expression(): StringParser[int] = Addition()

proc Addition(): StringParser[int] = Multiplication().chainl1(
  (s("+").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
    (proc(lhs: int, rhs: int): int = lhs + rhs))) /
  (s("-").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
    (proc(lhs: int, rhs: int): int = lhs - rhs)))
)

proc Multiplication(): StringParser[int] = Parenthesis().chainl(
  (s("*").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
    (proc(lhs: int, rhs: int): int = lhs * rhs))) /
  (s("/").map(proc(_: string): (proc(lhs: int, rhs: int): int) =
    (proc(lhs: int, rhs: int): int = lhs div rhs)))
)

proc Parenthesis(): StringParser[int] =
  regex(r"\s*\(\s*").flatMap(proc(_: string): StringParser[int] =
    Expression().flatMap(proc(e: int): StringParser[int] =
      regex(r"\s*\)\s*").map(proc(_: string): int =
        e))) / Number()

proc Number(): Parser[int, string] = regex(r"\s*[0-9]+\s*").map(proc(n: string): int =
  parseInt(n.strip()))


echo "Result of (1+2)*(3+4) is:"
echo parse(Expression(), "( 1 + 2 ) * ( 3 + 4 )")
echo "This will throw an error:"
echo parse(Expression(), "( 1 + 2 ) \n * \n ( 3 + 4 ")
