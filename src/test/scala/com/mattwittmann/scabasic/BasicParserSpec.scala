package com.mattwittmann.scabasic

import org.scalatest.{Assertions, WordSpec}

class BasicParserSpec extends WordSpec with Assertions {

  val parser = new BasicParser {}

  "A statement" when {
    "a comment" should {
      "return a remark command with the comment." in {
        assert(parser("10 REM This is a comment.") == Map(Label("10") -> Comment("This is a comment.")))
      }
    }
    "an assignment" should {
      "return an assignment AST." in {
        assert(parser("10 LET aVariable = 22") == Map(Label("10") -> VariableDeclaration(Identifier("aVariable"), NumericLiteral("22"))))
      }
      "hold a variable" in {
        assert(parser("10 LET aVariable = b") === Map(Label("10") -> VariableDeclaration(Identifier("aVariable"), Identifier("b"))))
      }
      "hold an ADT for 2 + 2" in {
        assert(parser("10 LET aVariable = 2 + 2") === Map(Label("10") -> VariableDeclaration(Identifier("aVariable"), Plus(NumericLiteral("2"), NumericLiteral("2")))))
      }
    }
    "a Print command" should {
      "print the numeric literal" in {
        assert(parser("10 PRINT 22") === Map(Label("10") -> Print(NumericLiteral("22"))))
      }
      "print, 'hi'" in {
        assert(parser("10 PRINT \"hi\"") === Map(Label("10") -> Print(StringLiteral("hi"))))
      }
      "print, 'Hello, world!'" in {
        assert(parser("10 PRINT \"Hello, world!\"") === Map(Label("10") -> Print(StringLiteral("Hello, world!"))))
      }
      "print the result of 2 + 2" in {
        assert(parser("10 PRINT 2 + 2") === Map(Label("10") -> Print(Plus(NumericLiteral("2"), NumericLiteral("2")))))
      }
      "print the result of 2 + 2 + 2" in {
        assert(parser("10 PRINT 2 + 2 + 2") === Map(Label("10") -> Print(Plus(NumericLiteral("2"), Plus(NumericLiteral("2"), NumericLiteral("2"))))))
      }
      "print not true" in {
        assert(parser("10 PRINT NOT TRUE") === Map(Label("10") -> Print(Not(BooleanTrue))))
      }
      "print not not true" in {
        assert(parser("10 PRINT NOT NOT TRUE") === Map(Label("10") -> Print(Not(Not(BooleanTrue)))))
      }
      "print (2 + 2)" in {
        assert(parser("10 PRINT (2 + 2)") === Map(Label("10") -> Print(Grouping(Plus(NumericLiteral("2"), NumericLiteral("2"))))))
      }
      "print 2 + (2 + 2)" in {
        assert(parser("10 PRINT 2 + (2 + 2)") === Map(Label("10") -> Print(Plus(NumericLiteral("2"), Grouping(Plus(NumericLiteral("2"), NumericLiteral("2")))))))
      }
      "print (2 + 2) + 2" in {
        assert(parser("10 PRINT (2 + 2) + 2") === Map(Label("10") -> Print(Plus(Grouping(Plus(NumericLiteral("2"), NumericLiteral("2"))), NumericLiteral("2")))))
      }
    }
    "A GoTo command" should {
      "go to Line 10" in {
        assert(parser("20 GOTO 10") === Map(Label("20") -> GoTo(Label("10"))))
      }
    }
    "An if clause" should {
      "print 2 if true" in {
        assert(parser("10 IF TRUE THEN PRINT 2") === Map(Label("10") -> If(BooleanTrue, Print(NumericLiteral("2")))))
      }
      "print 2 if 5 = 5" in {
        assert(parser("10 IF 5 = 5 THEN PRINT 2") === Map(Label("10") -> If(Equals(NumericLiteral("5"), NumericLiteral("5")), Print(NumericLiteral("2")))))
      }
      "print 2 + 2 if 5 = 5" in {
        assert(parser("10 IF 5 = 5 THEN PRINT 2 + 2") === Map(Label("10") -> If(Equals(NumericLiteral("5"), NumericLiteral("5")), Print(Plus(NumericLiteral("2"), NumericLiteral("2"))))))
      }
    }
  }
}
