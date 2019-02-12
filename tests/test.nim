import "../src/combparser"
import strutils

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
runTest("Test 16", peg("hello"), "hello", false)
runTest("Test 17", peg("hello"), "hello world", true)
