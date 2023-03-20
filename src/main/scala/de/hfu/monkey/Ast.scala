package de.hfu.monkey

sealed trait Node
sealed trait Statement extends Node
sealed trait Expression extends Node

case class Program(statements: List[Statement]) extends Node
case class Identifier(name: String) extends Expression
case class IntegerLiteral(value: Int) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
case class StringLiteral(value: String) extends Expression
case class ArrayLiteral(elements: List[Expression]) extends Expression
case class HashLiteral(pairs: Map[Expression, Expression]) extends Expression
case class LetStatement(name: Identifier, value: Expression) extends Statement
case class ReturnStatement(value: Expression) extends Statement
case class BlockStatement(statements: List[Statement]) extends Statement
case class IfExpression(condition: Expression, consequence: BlockStatement, alternative: BlockStatement) extends Expression
case class FunctionLiteral(parameters: List[Identifier], body: BlockStatement) extends Expression
case class CallExpression(function: Expression, arguments: List[Expression]) extends Expression
case class IndexExpression(left: Expression, index: Expression) extends Expression
case class ExpressionStatement(expression: Expression) extends Statement
case class PrefixExpression(operator: String, value: Expression) extends Expression
case class InfixExpression(operator: String, left: Expression, right: Expression) extends Expression
