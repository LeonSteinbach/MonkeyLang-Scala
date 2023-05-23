package de.hfu.monkey.parser

import de.hfu.monkey.*
import de.hfu.monkey.ast.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class CombinatorParser() extends parser.Parser with JavaTokenParsers {

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

	private def function: Parser[FunctionLiteral] = "fn" ~> ("(" ~> repsep(identifier, ",") <~ ")") ~ blockStatement ^^ {
		case parameters ~ body => FunctionLiteral(parameters, body)
	}

	private def value: Parser[Expression] = identifier | integer | boolean | string | array | hash

	private def ifExpression: Parser[IfExpression] = "if" ~> ("(" ~> expression <~ ")") ~ blockStatement ~ opt("else" ~> blockStatement) ^^ {
		case condition ~ consequence ~ alternative => IfExpression(condition, consequence, alternative.getOrElse(BlockStatement(Nil)))
	}

	private def callExpression(operandParser: Parser[Expression]): Parser[Expression] = {
		operandParser ~ rep("(" ~> repsep(expression, ",") <~ ")") ^^ {
			case expression ~ list => (list foldLeft expression) {
				case (left, args) => CallExpression(left, args)
			}
		}
	}

	private def indexExpression(operandParser: Parser[Expression]): Parser[Expression] = {
		operandParser ~ rep("[" ~> expression <~ "]") ^^ {
			case expression ~ list => (list foldLeft expression) {
				case (left, right) => IndexExpression(left, right)
			}
		}
	}

	private def infixExpression(operatorParser: Parser[String], operandParser: Parser[Expression]): Parser[Expression] = {
		operandParser ~ rep(operatorParser ~ operandParser) ^^ {
			case expression ~ list => (list foldLeft expression) {
				case (left, op ~ right) => InfixExpression(op, left, right)
			}
		}
	}

	private def prefixExpression: Parser[PrefixExpression] = ("-" | "!") ~ factor ^^ {
		case operator ~ value => PrefixExpression(operator, value)
	}

	private def groupedExpression: Parser[Expression] = "(" ~> expression <~ ")"

	private def factor: Parser[Expression] = ifExpression | function | callExpression(baseExpression) | prefixExpression | value | groupedExpression

	private def multiplicativeExpression: Parser[Expression] = infixExpression("*" | "/", indexExpression(factor))

	private def additiveExpression: Parser[Expression] = infixExpression("+" | "-", multiplicativeExpression)

	private def comparativeExpression: Parser[Expression] = infixExpression("<" | ">", additiveExpression)

	private def equalityExpression: Parser[Expression] = infixExpression("==" | "!=", comparativeExpression)

	private def expression: Parser[Expression] = equalityExpression

	private def baseExpression: Parser[Expression] = prefixExpression | value | "(" ~> expression <~ ")"

	private def expressionStatement: Parser[ExpressionStatement] = expression <~ ";" ^^ {
		expression => ExpressionStatement(expression)
	}

	private def letStatement: Parser[LetStatement] = "let" ~> identifier ~ ("=" ~> expression <~ ";") ^^ {
		case name ~ value => value match {
			case function: FunctionLiteral =>
				function.name = name.name
				LetStatement(name, function)
			case expression: Expression => LetStatement(name, expression)
		}
	}

	private def returnStatement: Parser[ReturnStatement] = "return" ~> expression <~ ";" ^^ {
		value => ReturnStatement(value)
	}

	private def blockStatement: Parser[BlockStatement] = "{" ~> statementList <~ "}" ^^ {
		statements => BlockStatement(statements)
	}

	private def statementList: Parser[List[Statement]] = rep(statement)

	private def statement: Parser[Statement] = letStatement | returnStatement | expressionStatement | blockStatement

	private def program: Parser[Program] = statementList ^^ { statements => Program(statements) }

	override def parse(input: String): Program = parseAll(program, input).get
}
