package com.mattwittmann.scabasic

sealed abstract class Node(token: String) {
  override def toString = token
}

/** A line number. */
case class Label(token: String) extends Node(token)
/** A built-in BASIC command such as PRINT or REM. */
sealed abstract class Command(token: String) extends Node(token)
case class VariableDeclaration(identifier: Identifier, expression: Expression) extends Command(s"LET $identifier = $expression")
/** A comment in the source code. BASIC called these remarks. */
case class Comment(token: String) extends Command(token)
case class GoTo(label: Label) extends Command(s"GOTO $label")
case class If(condition: Expression, command: Command) extends Command(s"IF $condition THEN $command")
sealed abstract class Expression(token: String) extends Node(token)
sealed abstract class Value(token: String) extends Expression(token)
/** A variable name. */
case class Identifier(token: String) extends Value(token)
/** A literal value. */
sealed abstract class Literal(token: String) extends Value(token)
/** A literal string value in the source code. */
case class StringLiteral(token: String) extends Literal(token)
/** A literal numeric (integral or floating-point) literal value in the source code. */
case class NumericLiteral(token: String) extends Literal(token)
sealed abstract class BooleanLiteral(token: String) extends Literal(token)
case object BooleanTrue extends BooleanLiteral("TRUE")
case object BooleanFalse extends BooleanLiteral("FALSE")
/** A built-in operator. */
sealed abstract class Operator(token: String) extends Expression(token)
sealed abstract class UnnaryOperator(token: String, operand: Expression) extends Operator(token + operand.toString)
sealed abstract class BinaryOperator(token: String, lhs: Expression, rhs: Expression) extends Operator(s"${lhs.toString} $token ${rhs.toString}")
/** Equals for assignment and comparison. */
case class Equals(lhs: Expression, rhs: Expression) extends BinaryOperator("=", lhs, rhs)
/** Inequality operator. */
case class NotEqual(lhs: Expression, rhs: Expression) extends BinaryOperator("<>", lhs, rhs)
/** Addition operator. */
case class Plus(lhs: Expression, rhs: Expression) extends BinaryOperator("+", lhs, rhs)
/**
 * Subtraction operator.
 * TODO Unary negative operator?
 */
case class Minus(lhs: Expression, rhs: Expression) extends BinaryOperator("-", lhs, rhs)
/** Multiplication operator. */
case class Times(lhs: Expression, rhs: Expression) extends BinaryOperator("*", lhs, rhs)
/** Division operator. */
case class DividedBy(lhs: Expression, rhs: Expression) extends BinaryOperator("/", lhs, rhs)
/** Operator for concatenating two strings together. */
case class Concatenate(lhs: Expression, rhs: Expression) extends BinaryOperator("&", lhs, rhs)
/** Logical and operator. */
case class And(lhs: Expression, rhs: Expression) extends BinaryOperator("AND", lhs, rhs)
/** Logical or operator. */
case class Or(lhs: Expression, rhs: Expression) extends BinaryOperator("OR", lhs, rhs)
/** Logical not operator. */
case class Not(expression: Expression) extends UnnaryOperator("NOT", expression)
case class Grouping(expression: Expression) extends Expression(s"(${expression.toString})")
case class Print(expression: Expression) extends Command("PRINT " + expression.toString)