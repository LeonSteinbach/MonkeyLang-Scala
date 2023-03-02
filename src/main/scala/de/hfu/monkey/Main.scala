package de.hfu.monkey

import scala.util.parsing.combinator.RegexParsers

sealed trait Node
sealed trait Statement extends Node
sealed trait Expression extends Node

case class Program(statements: List[Statement]) extends Node
case class Identifier(name: String) extends Expression
case class IntegerLiteral(value: Int) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
case class LetStatement(name: Identifier, value: Expression) extends Statement
case class ReturnStatement(value: Expression) extends Statement
case class BlockStatement(statements: List[Statement]) extends Statement
case class IfExpression(condition: Expression, consequence: BlockStatement, alternative: BlockStatement) extends Expression
case class FunctionLiteral(parameters: List[Identifier], body: BlockStatement) extends Expression
case class CallExpression(function: Expression, arguments: List[Expression]) extends Expression
case class ExpressionStatement(expression: Expression) extends Statement
case class PrefixExpression(operator: String, value: Expression) extends Expression
case class InfixExpression(operator: String, left: Expression, right: Expression) extends Expression

class MonkeyParser extends RegexParsers {

  private def identifier: Parser[Identifier] = """[a-zA-Z_]\w*""".r ^^ {
    name => Identifier(name)
  }

  private def integer: Parser[IntegerLiteral] = """\d+""".r ^^ {
    value => IntegerLiteral(value.toInt)
  }

  private def boolean: Parser[BooleanLiteral] = "true|false".r ^^ {
    value => BooleanLiteral(value.toBoolean)
  }

  private def value: Parser[Expression] = identifier | integer | boolean

  private def prefixExpression: Parser[PrefixExpression] = ("-" | "!") ~ factor ^^ {
    case operator ~ value => PrefixExpression(operator, value)
  }

  private def factor: Parser[Expression] = prefixExpression | value | "(" ~> expression <~ ")"

  private def multiplicativeExpression: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case factor ~ list => (list foldLeft factor) {
      case (acc, op ~ next) => InfixExpression(op, acc, next)
    }
  }

  private def additiveExpression: Parser[Expression] = multiplicativeExpression ~ rep(("+" | "-") ~ multiplicativeExpression) ^^ {
    case term ~ list => (list foldLeft term) {
      case (acc, op ~ next) => InfixExpression(op, acc, next)
    }
  }

  private def expression: Parser[Expression] = ifExpression | functionLiteral | callExpression | additiveExpression ~ rep(("==" | "!=" | "<" | ">") ~ additiveExpression) ^^ {
    case equalityExpression ~ list => (list foldLeft equalityExpression) {
      case (acc, op ~ next) => InfixExpression(op, acc, next)
    }
  }

  private def expressionStatement: Parser[ExpressionStatement] = expression <~ ";" ^^ {
    expression => ExpressionStatement(expression)
  }

  private def letStatement: Parser[LetStatement] = "let" ~> identifier ~ ("=" ~> expression <~ ";") ^^ {
    case name ~ value => LetStatement(name, value)
  }

  private def returnStatement: Parser[ReturnStatement] = "return" ~> expression <~ ";" ^^ {
    value => ReturnStatement(value)
  }

  private def blockStatement = "{" ~> rep(statement) <~ "}" ^^ {
    statements => BlockStatement(statements)
  }

  private def ifExpression: Parser[Expression] = "if" ~> ("(" ~> expression <~ ")") ~ blockStatement ~ opt("else" ~> blockStatement) ^^ {
    case condition ~ consequence ~ Some(alternative) => IfExpression(condition, consequence, alternative)
    case condition ~ consequence ~ None => IfExpression(condition, consequence, BlockStatement(Nil))
  }

  private def functionLiteral: Parser[Expression] = "fn" ~> ("(" ~> repsep(identifier, ",") <~ ")") ~ blockStatement ^^ {
    case parameters ~ body => FunctionLiteral(parameters, body)
  }

  private def callExpression: Parser[Expression] = identifier ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
    case function ~ arguments => CallExpression(function, arguments)
  }

  private def statement: Parser[Statement] = letStatement | returnStatement | expressionStatement | blockStatement

  def program: Parser[Program] = rep(statement) ^^ {
    statements => Program(statements)
  }

}

object Main extends App {
  private val parser = new MonkeyParser()
  private val result = parser.parseAll(parser.program, "asdf(1, x, y > z == y > x);")
  println(result)
}
