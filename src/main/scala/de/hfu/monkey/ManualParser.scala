package de.hfu.monkey

import scala.collection.mutable.ListBuffer

enum Precedence {
	case LOWEST, EQUALS, LESSGREATER, SUM, PRODUCT, PREFIX, CALL
}

case class ManualParser(lexer: Lexer) {

	private var currentToken: Option[Token] = None
	private var peekToken: Option[Token] = None
	var errors: Seq[String] = Seq[String]()

	private val precedences: Map[TokenType, Precedence] = Map(
		TokenType.EQ -> Precedence.EQUALS,
		TokenType.NEQ -> Precedence.EQUALS,
		TokenType.LT -> Precedence.LESSGREATER,
		TokenType.GT -> Precedence.LESSGREATER,
		TokenType.PLUS -> Precedence.SUM,
		TokenType.MINUS -> Precedence.SUM,
		TokenType.ASTERIX -> Precedence.PRODUCT,
		TokenType.SLASH -> Precedence.PRODUCT,
		TokenType.LPAREN -> Precedence.CALL
	)

	private val prefixParseFunctions: Map[TokenType, () => Option[Node]] = Map(
		TokenType.IDENT -> (() => parseIdentifier: Option[Node]),
		TokenType.INT -> (() => parseIntegerLiteral: Option[Node]),
		TokenType.TRUE -> (() => parseBooleanLiteral: Option[Node]),
		TokenType.FALSE -> (() => parseBooleanLiteral: Option[Node]),
	)

	private val infixParseFunctions: Map[TokenType, Expression => Expression] = Map(

	)

	advanceTokens()

	def parseProgram(): Program = {
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
		peekToken = Some(lexer.nextToken())
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
		errors = errors :+ s"Expected next token to be $tokenType, got ${peekToken.get.tokenType} instead."
	}

	private def noPrefixParseFunctionError(tokenType: TokenType): Unit = {
		errors = errors :+ s"No prefix parse function for $tokenType."
	}

	private def integerParseError(int: String): Unit = {
		errors = errors :+ s"Could not parse $int as integer."
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
			case None =>
				noPrefixParseFunctionError(currentToken.get.tokenType)
				return None
		}
		var leftExpression: Option[Expression] = parsePrefix().asInstanceOf[Option[Expression]]

		while (peekToken.get.tokenType != TokenType.SEMICOLON && precedence.ordinal < peekPrecedence.ordinal) {
			val parseInfix = infixParseFunctions.get(peekToken.get.tokenType) match {
				case Some(function) => function
				case None => return leftExpression
			}
			advanceTokens()
			leftExpression = Some(parseInfix(leftExpression.get))
		}
		leftExpression
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

}
