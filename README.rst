combparser
===========
This library implements what is known as a parser combinator system. The idea
behind this is to define your language by writing small, composable parsers
and combining them into a larger logic. It was written mainly for the
protobuf library which needed a parser that could run on compile-time to
parse the protobuf specification. The original idea came from a small
example library written by kmizu:
https://gist.github.com/kmizu/2b10c2bf0ab3eafecc1a825b892482f3 but has been
extensively rewritten with more matchers, more combinators, and proper error
handling. It still has some way to go to be the user-friendly parser library
it was indented to be, but it works well enough for now. For some examples
on how to use the library see the examples folder. Most of the combinators
have been written to accept not only string input and output, but any type.
This means that this library could theoretically be used to parse not only
string data but other types of data as well. Currently however the error
reporting is only string based, but an alternate system is being considered.

This file is automatically generated from the documentation found in
combparser.nim. Use ``nim doc2 combparser.nim`` to get the full documentation.
