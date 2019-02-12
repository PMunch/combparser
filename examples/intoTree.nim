## This example shows how you can use combparser to parse strings into a tree
## representation.
import "../src/combparser", strutils

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

proc Number(): Parser[int, string]

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
        e))) / Number().map(proc(val: int): Node =
          Node(kind: Value, value: val))

proc Number(): Parser[int, string] = regex(r"\s*[0-9]+\s*").map(proc(n: string): int =
  parseInt(n.strip()))

echo parse(Expression(), "( 1 + 2 )  *   ( 3 + 4 )")
echo "-----------------------------------------"
echo parse(Expression(), " 1 + 2  *  3 + 4")
echo "-----------------------------------------"
# The following line will cause a parse error as it can't be completely parsed
#echo parse(Expression(), "1 + 2  *  3 + 4 Hello world")
