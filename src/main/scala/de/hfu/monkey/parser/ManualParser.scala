package de.hfu.monkey.parser

import de.hfu.monkey.*
import de.hfu.monkey.ast.*
import de.hfu.monkey.lexer.{Lexer, Token, TokenType}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

enum Precedence {
	case LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL, INDEX
}

case class ManualParser() extends parser.Parser {

	private var lexer: Option[Lexer] = None
	private var currentToken: Option[Token] = None
	private var peekToken: Option[Token] = None
	private var errorList: Seq[String] = Seq[String]()
	override def errors: Seq[String] = errorList

	private val precedences: Map[TokenType, Precedence] = Map(
		TokenType.EQ -> Precedence.EQUALS,
		TokenType.NEQ -> Precedence.EQUALS,
		TokenType.LT -> Precedence.LESSGREATER,
		TokenType.GT -> Precedence.LESSGREATER,
		TokenType.PLUS -> Precedence.SUM,
		TokenType.MINUS -> Precedence.SUM,
		TokenType.ASTERIX -> Precedence.PRODUCT,
		TokenType.SLASH -> Precedence.PRODUCT,
		TokenType.LPAREN -> Precedence.CALL,
		TokenType.LBRACKET -> Precedence.INDEX
	)

	private val prefixParseFunctions: Map[TokenType, () => Option[Node]] = Map(
		TokenType.IDENT -> (() => parseIdentifier: Option[Node]),
		TokenType.INT -> (() => parseIntegerLiteral: Option[Node]),
		TokenType.TRUE -> (() => parseBooleanLiteral: Option[Node]),
		TokenType.FALSE -> (() => parseBooleanLiteral: Option[Node]),
		TokenType.STRING -> (() => parseStringLiteral: Option[Node]),
		TokenType.LBRACKET -> (() => parseArrayLiteral: Option[Node]),
		TokenType.LBRACE -> (() => parseHashLiteral: Option[Node]),
		TokenType.BANG -> (() => parsePrefixExpression: Option[Node]),
		TokenType.MINUS -> (() => parsePrefixExpression: Option[Node]),
		TokenType.LPAREN -> (() => parseGroupedExpression: Option[Node]),
		TokenType.IF -> (() => parseIfExpression: Option[Node]),
		TokenType.FUNCTION -> (() => parseFunctionLiteral: Option[Node]),
	)

	private val infixParseFunctions: Map[TokenType, Expression => Option[Node]] = Map(
		TokenType.LPAREN -> ((expression: Expression) => parseCallExpression(expression): Option[Node]),
		TokenType.LBRACKET -> ((leftExpression: Expression) => parseIndexExpression(leftExpression): Option[Node]),
		TokenType.PLUS -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.MINUS -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.ASTERIX -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.SLASH -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.EQ -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.NEQ -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.LT -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
		TokenType.GT -> ((leftExpression: Expression) => parseInfixExpression(leftExpression): Option[Node]),
	)

	private def parseProgram: Program = {
		val statements = ListBuffer[Statement]()
		advanceTokens()

		while (currentToken.get.tokenType != TokenType.EOF) {
			parseStatement match {
				case Some(statement: Statement) => statements += statement
				case _ =>
			}
			advanceTokens()
		}
		Program(statements.toList)
	}

	private def advanceTokens(): Unit = {
		currentToken = peekToken
		peekToken = Some(lexer.get.nextToken())
	}

	private def expectPeek(tokenType: TokenType): Boolean = {
		if (peekToken.get.tokenType == tokenType) {
			advanceTokens()
			true
		}
		else {
			peekError(tokenType)
			false
		}
	}

	private def peekPrecedence: Precedence = precedences.getOrElse(peekToken.get.tokenType, Precedence.LOWEST)

	private def peekError(tokenType: TokenType): Unit = {
		errorList = errorList :+ s"Expected next token to be $tokenType, got ${peekToken.get.tokenType} instead."
	}

	private def noPrefixParseFunctionError(tokenType: TokenType): Unit = {
		errorList = errorList :+ s"No prefix parse function for $tokenType."
	}

	private def integerParseError(int: String): Unit = {
		errorList = errorList :+ s"Could not parse $int as integer."
	}

	private def parseStatement: Option[Statement] = {
		currentToken.get.tokenType match {
			case TokenType.LET => parseLetStatement
			case TokenType.RETURN => parseReturnStatement
			case _ => parseExpressionStatement
		}
	}

	private def parseLetStatement: Option[LetStatement] = {
		if (!expectPeek(TokenType.IDENT))
			return None
		val name: Identifier = Identifier(currentToken.get.literal)
		if (!expectPeek(TokenType.ASSIGN))
			return None
		advanceTokens()
		val value: Expression = parseExpression(Precedence.LOWEST) match {
			case Some(functionLiteral: FunctionLiteral) =>
				functionLiteral.name = name.name
				Some(functionLiteral).get
			case Some(expression: Expression) => Some(expression).get
			case None => return None
		}
		if (peekToken.get.tokenType == TokenType.SEMICOLON)
			advanceTokens()
		Some(LetStatement(name, value))
	}

	private def parseReturnStatement: Option[ReturnStatement] = {
		advanceTokens()
		val value: Expression = parseExpression(Precedence.LOWEST) match {
			case Some(expression: Expression) => Some(expression).get
			case None => return None
		}
		if (peekToken.get.tokenType == TokenType.SEMICOLON)
			advanceTokens()
		Some(ReturnStatement(value))
	}

	private def parseExpressionStatement: Option[ExpressionStatement] = {
		val expression: Expression = parseExpression(Precedence.LOWEST) match {
			case Some(expression: Expression) => Some(expression).get
			case None => return None
		}
		if (peekToken.get.tokenType == TokenType.SEMICOLON)
			advanceTokens()
		Some(ExpressionStatement(expression))
	}

	private def parseExpression(precedence: Precedence): Option[Expression] = {
		val parsePrefix = prefixParseFunctions.get(currentToken.get.tokenType) match {
			case Some(function) => function
			case _ =>
				noPrefixParseFunctionError(currentToken.get.tokenType)
				return None
		}
		var leftExpression: Expression = parsePrefix() match {
			case Some(expression: Expression) => Some(expression).get
			case _ => return None
		}

		while (peekToken.isDefined && peekToken.get.tokenType != TokenType.SEMICOLON && precedence.ordinal < peekPrecedence.ordinal) {
			val parseInfix = infixParseFunctions.get(peekToken.get.tokenType) match {
				case Some(function) => function
				case _ => return Some(leftExpression)
			}
			advanceTokens()
			leftExpression = parseInfix(Some(leftExpression).get) match {
				case Some(expression: Expression) => Some(expression).get
				case _ => return None
			}
		}
		Some(leftExpression)
	}

	private def parseIdentifier: Option[Identifier] = Some(Identifier(currentToken.get.literal))

	private def parseIntegerLiteral: Option[IntegerLiteral] = {
		currentToken.get.literal.toIntOption match {
			case Some(int: Int) => Some(IntegerLiteral(int))
			case None =>
				integerParseError(currentToken.get.literal)
				None
		}
	}

	private def parseBooleanLiteral: Option[BooleanLiteral] = Some(BooleanLiteral(currentToken.get.tokenType == TokenType.TRUE))

	private def parseStringLiteral: Option[StringLiteral] = Some(StringLiteral(currentToken.get.literal))

	private def parseArrayLiteral: Option[ArrayLiteral] = Some(ArrayLiteral(parseExpressionList(TokenType.RBRACKET).getOrElse(List[Expression]())))

	private def parseHashLiteral: Option[HashLiteral] = {
		val pairs = mutable.HashMap.empty[Expression, Expression]

		while (peekToken.get.tokenType != TokenType.RBRACE) {
			advanceTokens()
			val key: Expression = parseExpression(Precedence.LOWEST) match {
				case Some(key: Expression) => Some(key).get
				case None => return None
			}

			if (!expectPeek(TokenType.COLON))
				return None
			advanceTokens()

			val value: Expression = parseExpression(Precedence.LOWEST) match {
				case Some(value: Expression) => Some(value).get
				case None => return None
			}

			pairs += (key -> value)

			if (peekToken.get.tokenType != TokenType.RBRACE && !expectPeek(TokenType.COMMA))
				return None
		}
		if (!expectPeek(TokenType.RBRACE))
			return None
		Some(HashLiteral(pairs.toMap))
	}

	private def parseGroupedExpression: Option[Expression] = {
		advanceTokens()
		val expression: Expression = parseExpression(Precedence.LOWEST) match {
			case Some(expression: Expression) => Some(expression).get
			case None => return None
		}
		if (expectPeek(TokenType.RPAREN)) {
			Some(expression)
		} else {
			None
		}
	}

	private def parseIfExpression: Option[IfExpression] = {
		if (!expectPeek(TokenType.LPAREN))
			return None

		advanceTokens()
		val condition: Expression = parseExpression(Precedence.LOWEST) match {
			case Some(expression: Expression) => Some(expression).get
			case None => return None
		}

		if (!expectPeek(TokenType.RPAREN))
			return None

		if (!expectPeek(TokenType.LBRACE))
			return None

		val consequence: BlockStatement = parseBlockStatement match {
			case Some(blockStatement: BlockStatement) => Some(blockStatement).get
			case None => return None
		}

		if (peekToken.get.tokenType != TokenType.ELSE)
			return Some(IfExpression(condition, consequence, BlockStatement(List[Statement]())))

		advanceTokens()
		if (!expectPeek(TokenType.LBRACE))
			return None

		val alternative: BlockStatement = parseBlockStatement match {
			case Some(blockStatement: BlockStatement) => Some(blockStatement).get
			case None => return None
		}

		Some(IfExpression(condition, consequence, alternative))
	}

	private def parseBlockStatement: Option[BlockStatement] = {
		advanceTokens()
		val statements = ListBuffer[Statement]()
		while (currentToken.get.tokenType != TokenType.RBRACE && currentToken.get.tokenType != TokenType.EOF) {
			parseStatement match {
				case Some(statement) => statements += statement
				case _ =>
			}
			advanceTokens()
		}
		Some(BlockStatement(statements.toList))
	}

	private def parseFunctionLiteral: Option[FunctionLiteral] = {
		if (!expectPeek(TokenType.LPAREN))
			return None

		val parameters: List[Identifier] = parseFunctionParameters match {
			case Some(parameters: List[Identifier]) => Some(parameters).get
			case _ => return None
		}

		if (!expectPeek(TokenType.LBRACE))
			return None

		val body: BlockStatement = parseBlockStatement match {
			case Some(blockStatement: BlockStatement) => Some(blockStatement).get
			case None => return None
		}

		Some(FunctionLiteral(parameters, body))
	}

	private def parseFunctionParameters: Option[List[Identifier]] = {
		val parameters: ListBuffer[Identifier] = ListBuffer[Identifier]()
		if (peekToken.get.tokenType == TokenType.RPAREN) {
			advanceTokens()
			return Some(parameters.toList)
		}
		advanceTokens()
		parameters += Identifier(currentToken.get.literal)
		while (peekToken.get.tokenType == TokenType.COMMA) {
			advanceTokens()
			advanceTokens()
			parameters += Identifier(currentToken.get.literal)
		}
		if (expectPeek(TokenType.RPAREN)) {
			Some(parameters.toList)
		} else {
			None
		}
	}

	private def parsePrefixExpression: Option[PrefixExpression] = {
		val operator: String = currentToken.get.literal
		advanceTokens()
		val expression: Expression = parseExpression(Precedence.PREFIX) match {
			case Some(expression: Expression) => Some(expression).get
			case None => return None
		}
		Some(PrefixExpression(operator, expression))
	}

	private def parseInfixExpression(leftExpression: Expression): Option[InfixExpression] = {
		val operator: String = currentToken.get.literal
		val precedence: Precedence = precedences.getOrElse(currentToken.get.tokenType, Precedence.LOWEST)
		advanceTokens()
		parseExpression(precedence) match {
			case Some(rightExpression: Expression) => Some(InfixExpression(operator, leftExpression, Some(rightExpression).get))
			case None => None
		}
	}

	private def parseCallExpression(function: Expression): Option[CallExpression] = {
		parseExpressionList(TokenType.RPAREN) match {
			case Some(arguments: List[Expression]) => Some(CallExpression(function, Some(arguments).get))
			case None => None
		}
	}

	private def parseIndexExpression(left: Expression): Option[IndexExpression] = {
		advanceTokens()
		val indexExpression: Expression = parseExpression(Precedence.LOWEST) match {
			case Some(indexExpression: Expression) => Some(indexExpression).get
			case _ => return None
		}
		if (expectPeek(TokenType.RBRACKET)) Some(IndexExpression(left, indexExpression)) else None
	}

	private def parseExpressionList(end: TokenType): Option[List[Expression]] = {
		val expressions: ListBuffer[Expression] = ListBuffer[Expression]()
		if (peekToken.get.tokenType == end) {
			advanceTokens()
			return Some(expressions.toList)
		}
		advanceTokens()
		parseExpression(Precedence.LOWEST) match
			case Some(expression: Expression) => expressions += Some(expression).get
			case _ =>

		while (peekToken.get.tokenType == TokenType.COMMA) {
			advanceTokens()
			advanceTokens()
			parseExpression(Precedence.LOWEST) match
				case Some(expression: Expression) => expressions += Some(expression).get
				case _ => return None
		}

		if (expectPeek(end)) {
			Some(expressions.toList)
		} else {
			None
		}
	}

	override def parse(input: String): Program = {
		lexer = Some(Lexer(input))
		advanceTokens()
		parseProgram
	}
}
