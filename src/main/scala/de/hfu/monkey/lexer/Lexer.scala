package de.hfu.monkey.lexer

import scala.annotation.tailrec

case class Lexer(private val input: String) {
	private var position: Int = 0
	private var readPosition: Int = 0
	private var currentCharacter: Option[Byte] = None

	readCharacter()

	private def readCharacter(): Unit = {
		currentCharacter =
			if (readPosition >= input.length)
				Some(-1.toByte)
			else
				Some(input.charAt(readPosition).toByte)
		position = readPosition
		readPosition += 1
	}

	private def peekCharacter(): Option[Byte] = if (readPosition >= input.length) Some(-1.toByte) else Some(input.charAt(readPosition).toByte)

	def nextToken(): Token = {
		skipWhitespace()

		val token = currentCharacter match {
			case Some('=') => getDoubleCharacterToken(TokenType.EQ, TokenType.ASSIGN, '=')
			case Some('!') => getDoubleCharacterToken(TokenType.NEQ, TokenType.BANG, '=')
			case Some('+') => Token(TokenType.PLUS, "+")
			case Some('-') => Token(TokenType.MINUS, "-")
			case Some('*') => Token(TokenType.ASTERIX, "*")
			case Some('/') => Token(TokenType.SLASH, "/")
			case Some('<') => Token(TokenType.LT, "<")
			case Some('>') => Token(TokenType.GT, ">")
			case Some(':') => Token(TokenType.COLON, ":")
			case Some(';') => Token(TokenType.SEMICOLON, ";")
			case Some('(') => Token(TokenType.LPAREN, "(")
			case Some(')') => Token(TokenType.RPAREN, ")")
			case Some(',') => Token(TokenType.COMMA, ",")
			case Some('{') => Token(TokenType.LBRACE, "{")
			case Some('}') => Token(TokenType.RBRACE, "}")
			case Some('[') => Token(TokenType.LBRACKET, "[")
			case Some(']') => Token(TokenType.RBRACKET, "]")
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

	private def getDoubleCharacterToken(doubleCharTokenType: TokenType, singleCharTokenType: TokenType, currentChar: Char): Token = {
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
