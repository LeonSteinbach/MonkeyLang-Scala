package de.hfu.monkey.Parser

import de.hfu.monkey.*

import scala.util.parsing.combinator.RegexParsers

case class CombinatorParser() extends Parser.Parser, RegexParsers {

	override def errors: Seq[String] = Seq[String]()

	private def identifier: Parser[Identifier] = not("let" | "return" | "fn" | "true" | "false") ~> """[a-zA-Z_]\w*\b""".r ^^ {
		name => Identifier(name)
	}

	private def integer: Parser[IntegerLiteral] =
		"""\d+""".r ^^ {
			value => IntegerLiteral(value.toInt)
		}

	private def boolean: Parser[BooleanLiteral] = ("true" | "false") ^^ {
		value => BooleanLiteral(value.toBoolean)
	}

	private def value: Parser[Expression] = identifier | integer | boolean

	private def binaryExpression(operatorParser: Parser[String], operandParser: Parser[Expression]): Parser[Expression] = {
		operandParser ~ rep(operatorParser ~ operandParser) ^^ {
			case expression ~ list => (list foldLeft expression) {
				case (left, op ~ right) => InfixExpression(op, left, right)
			}
		}
	}

	private def unaryExpression: Parser[PrefixExpression] = ("-" | "!") ~ factor ^^ {
		case operator ~ value => PrefixExpression(operator, value)
	}

	private def factor: Parser[Expression] = ifExpression | functionLiteral | callExpression | unaryExpression | value | "(" ~> expression <~ ")"

	private def multiplicativeExpression: Parser[Expression] = binaryExpression("*" | "/", factor)

	private def additiveExpression: Parser[Expression] = binaryExpression("+" | "-", multiplicativeExpression)

	private def comparativeExpression: Parser[Expression] = binaryExpression("<" | ">", additiveExpression)

	private def expression: Parser[Expression] = binaryExpression("==" | "!=", comparativeExpression)

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

	private def program: Parser[Program] = rep(statement) ^^ {
		statements => Program(statements)
	}

	override def parse(input: String): Program = {
		parseAll(program, input).get
	}
}
