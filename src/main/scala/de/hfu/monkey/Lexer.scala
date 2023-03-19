package de.hfu.monkey

import scala.annotation.tailrec

case class Lexer(private val input: String) {
	private var position: Int = 0
	private var readPosition: Int = 0
	private var currentCharacter: Option[Byte] = None

	readCharacter()

	private def readCharacter(): Unit = {
		currentCharacter = if (readPosition >= input.length) Some(-1.toByte) else Some(input.charAt(readPosition).toByte)
		position = readPosition
		readPosition += 1
	}

	private def peekCharacter(): Option[Byte] = if (readPosition >= input.length) Some(-1.toByte) else Some(input.charAt(readPosition).toByte)

	def nextToken(): Token = {
		skipWhitespace()

		val token = currentCharacter match {
			case Some('=') => processDoubleCharacterToken(TokenType.EQ, TokenType.ASSIGN, '=')
			case Some('!') => processDoubleCharacterToken(TokenType.NEQ, TokenType.BANG, '=')
			case Some('+') => Token(TokenType.PLUS, currentCharacter.get.toChar.toString)
			case Some('-') => Token(TokenType.MINUS, currentCharacter.get.toChar.toString)
			case Some('*') => Token(TokenType.ASTERIX, currentCharacter.get.toChar.toString)
			case Some('/') => Token(TokenType.SLASH, currentCharacter.get.toChar.toString)
			case Some('<') => Token(TokenType.LT, currentCharacter.get.toChar.toString)
			case Some('>') => Token(TokenType.GT, currentCharacter.get.toChar.toString)
			case Some(';') => Token(TokenType.SEMICOLON, currentCharacter.get.toChar.toString)
			case Some('(') => Token(TokenType.LPAREN, currentCharacter.get.toChar.toString)
			case Some(')') => Token(TokenType.RPAREN, currentCharacter.get.toChar.toString)
			case Some(',') => Token(TokenType.COMMA, currentCharacter.get.toChar.toString)
			case Some('{') => Token(TokenType.LBRACE, currentCharacter.get.toChar.toString)
			case Some('}') => Token(TokenType.RBRACE, currentCharacter.get.toChar.toString)
			case Some('"') => Token(TokenType.STRING, readString())
			case Some(-1) => Token(TokenType.EOF, "")
			case _ => getIdentifier
		}
		readCharacter()
		token
	}

	@tailrec
	private def readString(oldPosition: Int = position + 1, found: Boolean = false): String = {
		if (currentCharacter.get == -1) {
			throw new NoSuchElementException("Unexpected end of input")
		} else if (found && currentCharacter.get == '"') {
			input.substring(oldPosition, position)
		} else {
			readCharacter()
			if (currentCharacter.get == '"') {
				readString(oldPosition, true)
			} else {
				readString(oldPosition, found)
			}
		}
	}

	private def getIdentifier: Token = {
		currentCharacter match {
			case Some(c) if c.toChar.isLetter =>
				val literal = currentCharacter.get.toChar.toString + readWord(_.isLetterOrDigit)
				Token(Token.lookupIdent(literal), literal)
			case Some(c) if c.toChar.isDigit =>
				val literal = currentCharacter.get.toChar.toString + readWord(_.isLetterOrDigit)
				if (literal.length > 1 && literal.startsWith("0"))
					Token(TokenType.ILLEGAL, literal)
				else
					Token(TokenType.INT, literal)
			case Some(c) if !c.toChar.isWhitespace =>
				Token(TokenType.ILLEGAL, c.toChar.toString)
			case _ =>
				Token(TokenType.ILLEGAL, "")
		}
	}

	private def processDoubleCharacterToken(doubleCharTokenType: TokenType, singleCharTokenType: TokenType, currentChar: Char): Token = {
		peekCharacter() match {
			case Some(c) if c == currentChar =>
				var literal = currentCharacter.get.toChar.toString
				readCharacter()
				literal += currentCharacter.get.toChar.toString
				Token(doubleCharTokenType, literal)
			case _ =>
				Token(singleCharTokenType, currentCharacter.get.toChar.toString)
		}
	}

	@tailrec
	private def readWord(condition: Char => Boolean, acc: String = ""): String = {
		peekCharacter() match {
			case Some(c) if condition(c.toChar) =>
				readCharacter()
				readWord(condition, acc + c.toChar)
			case _ =>
				acc
		}
	}

	@tailrec
	private def skipWhitespace(): Unit = {
		currentCharacter match {
			case Some(c) if c.toChar.isWhitespace =>
				readCharacter()
				skipWhitespace()
			case _ =>
		}
	}
}
