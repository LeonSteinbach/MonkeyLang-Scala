package de.hfu.monkey.parser

import de.hfu.monkey.*
import de.hfu.monkey.ast.*

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

case class CombinatorParser() extends parser.Parser with JavaTokenParsers {

	override def errors: Seq[String] = Seq[String]()

	private def identifier: Parser[Identifier] =
		not("let" | "if" | "else" | "return" | "fn" | "true" | "false") ~> """[a-zA-Z_]\w*\b""".r ^^ {
			name => Identifier(name)
		}

	private def integer: Parser[IntegerLiteral] = {
		"""\d+\w*""".r >> {
			case value if value.matches("(0|[1-9]+\\d*)") => success(IntegerLiteral(value.toInt))
			case invalidInput => err(s"Could not parse '$invalidInput' as integer.")
		}
	}

	private def boolean: Parser[BooleanLiteral] = ("true" | "false") ^^ {
		value => BooleanLiteral(value.toBoolean)
	}

	private def string: Parser[StringLiteral] = stringLiteral ^^ {
		value => StringLiteral(value.substring(1, value.length - 1))
	}

	private def array: Parser[ArrayLiteral] = "[" ~> repsep(expression, ",") <~ "]" ^^ {
		elements => ArrayLiteral(elements)
	}

	private def hash: Parser[HashLiteral] = "{" ~> repsep((expression <~ ":") ~ expression, ",") <~ "}" ^^ {
		keyValuePairs =>
			val pairs = keyValuePairs.map {
				case key ~ value => (key, value)
			}
			HashLiteral(pairs.toMap)
		}

	private def function: Parser[FunctionLiteral] =
		"fn" ~> ("(" ~> repsep(identifier, ",") <~ ")") ~ blockStatement ^^ {
			case parameters ~ body => FunctionLiteral(parameters, body)
		}

	private def value: Parser[Expression] = integer | boolean | string | array | hash

	private def ifExpression: Parser[IfExpression] = "if" ~> ("(" ~> expression <~ ")") ~ blockStatement ~ opt("else" ~> blockStatement) ^^ {
		case condition ~ consequence ~ alternative => IfExpression(condition, consequence, alternative.getOrElse(BlockStatement(Nil)))
	}

	private def primaryExpression: Parser[Expression] = ifExpression | function | groupedExpression | identifier | value

	private def groupedExpression: Parser[Expression] = "(" ~> expression <~ ")"

	private def infixExpression(operatorParser: Parser[String], operandParser: Parser[Expression]): Parser[Expression] =
		operandParser ~ rep(operatorParser ~ operandParser) ^^ {
			case expression ~ list => (list foldLeft expression) {
				case (left, operator ~ right) => InfixExpression(operator, left, right)
			}
		}

	private def expression: Parser[Expression] = equalityExpression

	private def equalityExpression: Parser[Expression] = infixExpression("==" | "!=", comparativeExpression)

	private def comparativeExpression: Parser[Expression] = infixExpression("<" | ">", additiveExpression)

	private def additiveExpression: Parser[Expression] = infixExpression("+" | "-", multiplicativeExpression)

	private def multiplicativeExpression: Parser[Expression] = infixExpression("*" | "/", prefixExpression)

	private def prefixExpression: Parser[Expression] =
		rep("-" | "!") ~ postfixExpression ^^ {
			case operators ~ expression =>
				(operators foldRight expression) {
					(operator, expression) => PrefixExpression(operator, expression)
				}
		}

	private def postfixExpression: Parser[Expression] =
		primaryExpression ~ rep(postfixOperator) ^^ {
			case left ~ list => (list foldLeft left) {
				case (left, (operator, right)) => operator match {
					case "(" => CallExpression(left, right.asInstanceOf[List[Expression]])
					case "[" => IndexExpression(left, right.asInstanceOf[Expression])
				}
			}
		}

	private def postfixOperator: Parser[(String, Any)] =
		"(" ~> repsep(expression, ",") <~ ")" ^^ { args => ("(", args) } |
		"[" ~> expression <~ "]" ^^ { expr => ("[", expr) }

	private def expressionStatement: Parser[ExpressionStatement] = expression <~ ";" ^^ {
		expression => ExpressionStatement(expression)
	}

	private def letStatement: Parser[LetStatement] = "let" ~> identifier ~ ("=" ~> expression <~ ";") ^^ {
		case name ~ expression => expression match {
			case function: FunctionLiteral =>
				function.name = name.name
				LetStatement(name, function)
			case expression: Expression => LetStatement(name, expression)
		}
	}

	private def returnStatement: Parser[ReturnStatement] = "return" ~> expression <~ ";" ^^ {
		expression => ReturnStatement(expression)
	}

	private def blockStatement: Parser[BlockStatement] = "{" ~> statementList <~ "}" ^^ {
		statements => BlockStatement(statements)
	}

	private def statementList: Parser[List[Statement]] = rep(statement)

	private def statement: Parser[Statement] = letStatement | returnStatement | expressionStatement

	private def program: Parser[Program] = statementList ^^ { statements => Program(statements) }

	override def parse(input: String): Program = {
		val parseResult = parseAll(program, input)
		parseResult match {
			case Success(parsed, _) => parsed
			case error: NoSuccess =>
				val errorPosition = error.next.pos
				val errorMessage = s"Parsing error at line ${errorPosition.line}, column ${errorPosition.column}: ${error.msg}"
				println(errorMessage + "\n")
				throw new ParserException("Parsing failed with errors.")
		}
	}

}
