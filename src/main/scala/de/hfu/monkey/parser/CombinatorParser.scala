package de.hfu.monkey.parser

import de.hfu.monkey.*
import de.hfu.monkey.ast.{ArrayLiteral, BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, Statement, StringLiteral}

import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

case class CombinatorParser() extends parser.Parser, JavaTokenParsers {

	override def errors: Seq[String] = Seq[String]()

	private def identifier: Parser[Identifier] = not("let" | "return" | "fn" | "true" | "false") ~> """[a-zA-Z_]\w*\b""".r ^^ { name => Identifier(name) }

	private def integer: Parser[IntegerLiteral] = wholeNumber ^^ { value => IntegerLiteral(value.toInt) }

	private def boolean: Parser[BooleanLiteral] = ("true" | "false") ^^ { value => BooleanLiteral(value.toBoolean) }

	private def string: Parser[StringLiteral] = stringLiteral ^^ { value => StringLiteral(value.substring(1, value.length - 1) ) }

	private def array: Parser[ArrayLiteral] = "[" ~> repsep(expression, ",") <~ "]" ^^ { elements => ArrayLiteral(elements) }

	private def hash: Parser[HashLiteral] = "{" ~> repsep((expression <~ ":") ~ expression, ",") <~ "}" ^^ { keyValuePairs =>
		val pairs = keyValuePairs.map {
			case key ~ value => (key, value)
		}
		HashLiteral(pairs.toMap)
	}

	private def value: Parser[Expression] = identifier | integer | boolean | string | array | hash

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

	private def indexExpression(operandParser: Parser[Expression]): Parser[Expression] = {
		operandParser ~ rep("[" ~> expression <~ "]") ^^ {
			case expression ~ list => (list foldLeft expression) {
				case (left, right) => IndexExpression(left, right)
			}
		}
	}

	private def multiplicativeExpression: Parser[Expression] = binaryExpression("*" | "/", indexExpression(factor))

	private def additiveExpression: Parser[Expression] = binaryExpression("+" | "-", multiplicativeExpression)

	private def comparativeExpression: Parser[Expression] = binaryExpression("<" | ">", additiveExpression)

	private def equalityExpression: Parser[Expression] = binaryExpression("==" | "!=", comparativeExpression)

	private def expression: Parser[Expression] = equalityExpression

	private def expressionStatement: Parser[ExpressionStatement] = expression <~ ";" ^^ {
		expression => ExpressionStatement(expression)
	}

	private def letStatement: Parser[LetStatement] = "let" ~> identifier ~ ("=" ~> expression <~ ";") ^^ {
		case name ~ value => LetStatement(name, value)
	}

	private def returnStatement: Parser[ReturnStatement] = "return" ~> expression <~ ";" ^^ {
		value => ReturnStatement(value)
	}

	private def blockStatement: Parser[BlockStatement] = "{" ~> rep(statement) <~ "}" ^^ {
		statements => BlockStatement(statements)
	}

	private def ifExpression: Parser[IfExpression] = "if" ~> ("(" ~> expression <~ ")") ~ blockStatement ~ opt("else" ~> blockStatement) ^^ {
		case condition ~ consequence ~ alternative => IfExpression(condition, consequence, alternative.getOrElse(BlockStatement(Nil)))
	}

	private def functionLiteral: Parser[FunctionLiteral] = "fn" ~> ("(" ~> repsep(identifier, ",") <~ ")") ~ blockStatement ^^ {
		case parameters ~ body => FunctionLiteral(parameters, body)
	}

	private def callExpression: Parser[CallExpression] = identifier ~ ("(" ~> repsep(expression, ",") <~ ")") ^^ {
		case function ~ arguments => CallExpression(function, arguments)
	}

	//private def indexExpression: Parser[Expression] = (ifExpression | functionLiteral | callExpression | unaryExpression | value | "(" ~> expression <~ ")") ~ ("[" ~> expression <~ "]") ^^ {
	//	case left ~ right => IndexExpression(left, right)
	//}

	private def statement: Parser[Statement] = letStatement | returnStatement | expressionStatement | blockStatement

	private def program: Parser[Program] = rep(statement) ^^ { statements => Program(statements) }

	override def parse(input: String): Program = parseAll(program, input).get
}
