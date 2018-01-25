const
  termBlack* = "\e[30m"
  termRed* = "\e[31m"
  termGreen* = "\e[32m"
  termYellow* = "\e[33m"
  termBlue* = "\e[34m"
  termMagenta* = "\e[35m"
  termCyan* = "\e[36m"
  termWhite* = "\e[37m"
  termBgBlack* = "\e[40m"
  termBgRed* = "\e[41m"
  termBgGreen* = "\e[42m"
  termBgYellow* = "\e[43m"
  termBgBlue* = "\e[44m"
  termBgMagenta* = "\e[45m"
  termBgCyan* = "\e[46m"
  termBgWhite* = "\e[47m"
  termClear* = "\e[0m"
  termBold = "\e[1m"
  termItalic = "\e[3m"
  termUnderline = "\e[4m"
  termBlink = "\e[5m"
  termNegative = "\e[7m"
  termStrikethrough = "\e[9m"

template addEnd(ss: varargs[string, `$`]): untyped =
  for s in ss:
    result &= s
  result &= termClear

proc black*(ss: varargs[string, `$`]): string =
  result = termBlack
  addEnd(ss)

proc red*(ss: varargs[string, `$`]): string =
  result = termRed
  addEnd(ss)

proc green*(ss: varargs[string, `$`]): string =
  result = termGreen
  addEnd(ss)

proc yellow*(ss: varargs[string, `$`]): string =
  result = termYellow
  addEnd(ss)

proc blue*(ss: varargs[string, `$`]): string =
  result = termBlue
  addEnd(ss)

proc magenta*(ss: varargs[string, `$`]): string =
  result = termMagenta
  addEnd(ss)

proc cyan*(ss: varargs[string, `$`]): string =
  result = termCyan
  addEnd(ss)

proc white*(ss: varargs[string, `$`]): string =
  result = termWhite
  addEnd(ss)

proc bgBlack*(ss: varargs[string, `$`]): string =
  result = termBgBlack
  addEnd(ss)

proc bgRed*(ss: varargs[string, `$`]): string =
  result = termBgRed
  addEnd(ss)

proc bgGreen*(ss: varargs[string, `$`]): string =
  result = termBgGreen
  addEnd(ss)

proc bgYellow*(ss: varargs[string, `$`]): string =
  result = termBgYellow
  addEnd(ss)

proc bgBlue*(ss: varargs[string, `$`]): string =
  result = termBgBlue
  addEnd(ss)

proc bgMagenta*(ss: varargs[string, `$`]): string =
  result = termBgMagenta
  addEnd(ss)

proc bgCyan*(ss: varargs[string, `$`]): string =
  result = termBgCyan
  addEnd(ss)

proc bgWhite*(ss: varargs[string, `$`]): string =
  result = termBgWhite
  addEnd(ss)

proc bold*(ss: varargs[string, `$`]): string =
  result = termBold
  addEnd(ss)

proc italic*(ss: varargs[string, `$`]): string =
  result = termItalic
  addEnd(ss)

proc underline*(ss: varargs[string, `$`]): string =
  result = termUnderline
  addEnd(ss)

proc blink*(ss: varargs[string, `$`]): string =
  result = termBlink
  addEnd(ss)

proc negative*(ss: varargs[string, `$`]): string =
  result = termNegative
  addEnd(ss)

proc strikethrough*(ss: varargs[string, `$`]): string =
  result = termStrikethrough
  addEnd(ss)

proc style*(ss: varargs[string, `$`], style: string): string =
  result = style
  addEnd(ss)