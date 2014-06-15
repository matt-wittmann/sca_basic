package com.mattwittmann.scabasic

import scala.util.parsing.combinator.RegexParsers

trait BasicParser extends RegexParsers {
  def anyString: Parser[String] = ".*".r ^^ { _.toString }
  def label: Parser[Label] = """\d+""".r ^^ { Label(_) }
  def comment: Parser[Comment] = "REM" ~ anyString ^^ { case "REM" ~ remark => Comment(remark) }
  def print: Parser[Print] = "PRINT" ~ expression ^^ { case "PRINT" ~ expression => Print(expression) }
  def identifier: Parser[Identifier] = """[a-zA-Z][\w\d]*""".r ^^ { Identifier(_) }
  def number: Parser[NumericLiteral] = """-?\d+(\.\d*)?""".r ^^ { NumericLiteral(_) }
  def booleanTrue: Parser[BooleanLiteral] = "TRUE" ^^^ BooleanTrue
  def booleanFalse: Parser[BooleanLiteral] = "FALSE" ^^^ BooleanFalse
  def boolean: Parser[BooleanLiteral] = booleanTrue | booleanFalse
  def string: Parser[StringLiteral] = "\"" ~ """[\w\d ,!\t\-@$\*\(\)\^%\+']*""".r ~ "\"" ^^ { case "\"" ~ string ~ "\"" => StringLiteral(string) }
  def literal: Parser[Literal] = number | boolean | string
  def value: Parser[Expression] = literal | identifier | grouping

  def assignment: Parser[VariableDeclaration] = "LET" ~ identifier ~ "=" ~ expression ^^ {
    case "LET" ~ identifier ~ "=" ~ expression => VariableDeclaration(identifier, expression)
  }

  def goto: Parser[GoTo] = "GOTO" ~ label ^^ { case "GOTO" ~ label => GoTo(label) }
  def ifThen: Parser[If] = "IF" ~ expression ~ "THEN" ~ command ^^ { case "IF" ~ condition ~ "THEN" ~ command => If(condition, command) }

  def not: Parser[Not] = "NOT" ~ expression ^^ { case "NOT" ~ operand => Not(operand) }

  def binaryOperator: Parser[String] = "+" | "-" | "*" | "/" | "&" | "AND" | "OR" | "=" | "<>"
  def unaryOperator: Parser[String] = "NOT"

  def grouping: Parser[Expression] = log("(" ~> expression <~ ")")("grouped") ^^ { Grouping(_) }

  def prefixedExpression: Parser[Expression] = rep(unaryOperator) ~ value ^^ {
    case list ~ operand => (operand /: list) {
      case (operand, "NOT") => Not(operand)
      case (operand, _) => operand
    }
  }

  def expression: Parser[Expression] = prefixedExpression ~ rep(binaryOperator ~ expression) ^^ {
    case lhs ~ list => ((lhs: Expression) /: list) {
      case (lhs, "+" ~ rhs) => Plus(lhs, rhs)
      case (lhs, "-" ~ rhs) => Minus(lhs, rhs)
      case (lhs, "*" ~ rhs) => Times(lhs, rhs)
      case (lhs, "/" ~ rhs) => DividedBy(lhs, rhs)
      case (lhs, "&" ~ rhs) => Concatenate(lhs, rhs)
      case (lhs, "AND" ~ rhs) => And(lhs, rhs)
      case (lhs, "OR" ~ rhs) => Or(lhs, rhs)
      case (lhs, "=" ~ rhs) => Equals(lhs, rhs)
      case (lhs, "<>" ~ rhs) => NotEqual(lhs, rhs)
      case (lhs,  _) => lhs
    }
  }

  def command: Parser[Command] = comment | assignment | print | goto | ifThen

  def statement: Parser[Map[Label, Node]] = label ~ command ^^ {
    case label ~ rest => Map(label -> rest)
  }

  def parseNode(function: this.type#Parser[Node], input: String): Node = parseAll(function, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  def apply(input: String): Map[Label, Node] = parseAll(statement, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }
}
