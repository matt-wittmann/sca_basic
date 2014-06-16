package com.mattwittmann.scabasic

import scala.util.parsing.combinator.RegexParsers

/**
 * Parses retro-style BASIC commands into an abstract syntax tree using the Scala Parser Combinator module's `RegexParsers` trait.
 */
trait BasicParser extends RegexParsers {
  /** Matches any string. */
  def anyString: Parser[String] = ".*".r ^^ { _.toString }
  /** Line number to go to from elsewhere. */
  def label: Parser[Label] = """\d+""".r ^^ { Label(_) }
  /** A remark in the code. */
  def comment: Parser[Comment] = "REM" ~ anyString ^^ { case "REM" ~ remark => Comment(remark) }
  /** Command to print a string to STDOUT. */
  def print: Parser[Print] = "PRINT" ~ expression ^^ { case "PRINT" ~ expression => Print(expression) }
  /** An identifier for variables. */
  def identifier: Parser[Identifier] = """[a-zA-Z][\w\d]*""".r ^^ { Identifier(_) }
  /** A numeric literal, an integer or decimal. */
  def number: Parser[NumericLiteral] = """-?\d+(\.\d*)?""".r ^^ { NumericLiteral(_) }
  /** Literal `TRUE`. */
  def booleanTrue: Parser[BooleanLiteral] = "TRUE" ^^^ BooleanTrue
  /** Literal `FALSE`. */
  def booleanFalse: Parser[BooleanLiteral] = "FALSE" ^^^ BooleanFalse
  /** Boolean literal. */
  def boolean: Parser[BooleanLiteral] = booleanTrue | booleanFalse

  /**
   * Parses a string literal.
   * TODO This is not a comprehensive treatment of escape sequences and special characters.
   *
   * @return A [[StringLiteral]] lifted into a Parser context
   */
  def string: Parser[StringLiteral] = "\"" ~ """[\w\d ,!\t\-@$\*\(\)\^%\+']*""".r ~ "\"" ^^ { case "\"" ~ string ~ "\"" => StringLiteral(string) }
  /** The different literal values this parser supports. */
  def literal: Parser[Literal] = number | boolean | string
  /** Things that can be used like a value. */
  def value: Parser[Expression] = literal | identifier | grouping

  /**
   * Assigns a value to a variable.
   *
   * @return Lifts a [[VariableDeclaration]] into a Parser context
   */
  def assignment: Parser[VariableDeclaration] = "LET" ~ identifier ~ "=" ~ expression ^^ {
    case "LET" ~ identifier ~ "=" ~ expression => VariableDeclaration(identifier, expression)
  }

  /** Parses a `GOTO` [[Label]]. */
  def goto: Parser[GoTo] = "GOTO" ~ label ^^ { case "GOTO" ~ label => GoTo(label) }

  /** An IF...THEN statement. */
  def ifThen: Parser[If] = "IF" ~ expression ~ "THEN" ~ command ^^ { case "IF" ~ condition ~ "THEN" ~ command => If(condition, command) }

  /** The various binary infix operators in BASIC. */
  def binaryOperator: Parser[String] = "+" | "-" | "*" | "/" | "&" | "AND" | "OR" | "=" | "<>"
  /** Unary operators, namely the logic operator NOT. */
  def unaryOperator: Parser[String] = "NOT"

  /**
   * A [[Grouping]], those parantheses, for an expression.
   *
   * @return An [[Expression]] lifted into a Parser context
   */
  def grouping: Parser[Expression] = "(" ~> expression <~ ")" ^^ { Grouping(_) }

  /**
   * Some operators, namely [[Not]], act are prefixed to an expression.
   *
   * @return An [[Expression]] lifted into a Parser context
   */
  def prefixedExpression: Parser[Expression] = rep(unaryOperator) ~ value ^^ {
    case list ~ operand => (operand /: list) {
      case (operand, "NOT") => Not(operand)
      case (operand, _) => operand
    }
  }

  /**
   * Parses a BASIC expression.
   *
   * @return An [[Expression]] lifted into a Parser context
   */
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

  /**
   * Parses a BASIC command like LET, PRINT, or IF.
   *
   * @return A [[Command]] lifted into a Parser context.
   */
  def command: Parser[Command] = comment | assignment | print | goto | ifThen

  /**
   * Parses a BASIC statement.
   *
   * @return A map of labels (line numbers) to BASIC ADT nodes lifted into a Scala parser object
   */
  def statement: Parser[Map[Label, Node]] = label ~ command ^^ {
    case label ~ rest => Map(label -> rest)
  }

  /**
   * Adds statements/maps together.
   *
   * @return A map of labels (line numbers) to BASIC ADT nodes lifted into a Scala parser object
   */
  def statements: Parser[Map[Label, Node]] = rep(statement) ^^ {
    // TODO Use semicolon for multiple statements on one line?
    // """;|\n|\r\n?|\z""".r
    case list => list.reduce(_ ++ _)
  }

  /**
   * Useful for working with other parser combinators besides taking a whole statement at a time.
   *
   * @param function A parser function in [[BasicParser]]
   * @param input The BASIC code fragment to try to parse
   * @return Returns a node in the abstract syntax tree
   */
  def parseNode(function: this.type#Parser[Node], input: String): Node = parseAll(function, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }

  /**
   * Apply a BasicParser object like a function.
   *
   * @param input A single line of BASIC code (right now)
   * @return A map of labels (line numbers) to BASIC statements
   */
  def apply(input: String): Map[Label, Node] = parseAll(statements, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => sys.error(failure.msg)
  }
}
