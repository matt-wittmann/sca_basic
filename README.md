sca_basic
=========

Introduction
------------

sca_basic is a simple retro-style BASIC interpreter written in the Scala programming language using Scala's
parser combinators module. The vintage is pre-GW-BASIC and roughly based on the description of Dartmouth BASIC.

Labels (line numbers) indicate a BASIC command to follow (the statements are then ordered by this line number!);
the interpreter assumes anything else is a command to the interpreter itself like exiting the interpreter.

This is mainly meant as an exercise in parser combinators and as a send-out to the recently-turned-50-year-old language.

Examples
--------

### Tests

com.mattwittmann.scabasic.BasicParserSpec has test cases for the parser, which builds an abstract syntax tree.
The interpreter of this ADT is a to-do.

### Syntax

The BASIC dialect is pre-structured and the very antithesis of everything Scala stands for: dynamically typed,
not functional, not object oriented, not structured at all:

    10 LET X = 5
    20 PRINT X + 3
    30 GOTO 10
    40 REM Infinite loop