package de.hfu.monkey

case class Lexer(private val input: String) {
	private var position: Int = 0
	private var readPosition: Int = 0
	private var currentCharacter: Byte = 0

	private def readCharacter(): Unit = {
		currentCharacter = if (readPosition >= input.length) -1 else input.charAt(readPosition).toByte
		position = readPosition
		readPosition += 1
	}

	private def peekCharacter(): Byte = if (readPosition >= input.length) -1 else input.charAt(readPosition).toByte

	def nextToken(): Token = {
		skipWhitespace()

		val token = currentCharacter match {
			case '=' => processDoubleCharacterToken(TokenType.EQ, TokenType.ASSIGN, '=')
			case '!' => processDoubleCharacterToken(TokenType.NEQ, TokenType.BANG, '=')
			case '+' => Token(TokenType.PLUS, currentCharacter.toChar.toString)
			case '-' => Token(TokenType.MINUS, currentCharacter.toChar.toString)
			case '*' => Token(TokenType.ASTERIX, currentCharacter.toChar.toString)
			case '/' => Token(TokenType.SLASH, currentCharacter.toChar.toString)
			case '<' => Token(TokenType.LT, currentCharacter.toChar.toString)
			case '>' => Token(TokenType.GT, currentCharacter.toChar.toString)
			case ';' => Token(TokenType.SEMICOLON, currentCharacter.toChar.toString)
			case '(' => Token(TokenType.LPAREN, currentCharacter.toChar.toString)
			case ')' => Token(TokenType.RPAREN, currentCharacter.toChar.toString)
			case ',' => Token(TokenType.COMMA, currentCharacter.toChar.toString)
			case '{' => Token(TokenType.LBRACE, currentCharacter.toChar.toString)
			case '}' => Token(TokenType.RBRACE, currentCharacter.toChar.toString)
			case -1 => Token(TokenType.EOF, "")
			case _ => getIdentifier
		}
		readCharacter()
		token
	}

	private def getIdentifier: Token = {
		if (currentCharacter.toChar.isLetter) {
			val literal = readIdentifierOrNumber
			Token(Token.lookupIdent(literal), literal)
		} else if (currentCharacter.toChar.isDigit) {
			val literal = readIdentifierOrNumber
			if (literal.length > 1 && (literal.startsWith("0") || literal.exists(_.isLetter)))
				Token(TokenType.ILLEGAL, literal)
			else
				Token(TokenType.INT, literal)
		} else {
			Token(TokenType.ILLEGAL, currentCharacter.toChar.toString)
		}
	}

	private def processDoubleCharacterToken(doubleCharTokenType: TokenType, singleCharTokenType: TokenType, currentChar: Char): Token = {
		if (peekCharacter() == currentChar) {
			var literal = currentCharacter.toChar.toString
			readCharacter()
			literal += currentCharacter.toChar.toString
			Token(doubleCharTokenType, literal)
		} else {
			Token(singleCharTokenType, currentCharacter.toChar.toString)
		}
	}

	private def readIdentifierOrNumber: String = {
		val oldPosition = position
		while (peekCharacter().toChar.isLetter || peekCharacter().toChar.isDigit)
			readCharacter()
		input.substring(oldPosition, position + 1)
	}

	private def skipWhitespace(): Unit = {
		while (List(' ', '\t', '\n', '\r').contains(currentCharacter.toChar))
			readCharacter()
	}
}
