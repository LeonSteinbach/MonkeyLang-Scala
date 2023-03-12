package de.hfu.monkey

import scala.annotation.tailrec

case class Lexer(private val input: String) {
	private var position: Int = 0
	private var readPosition: Int = 0
	private var currentCharacter: Option[Byte] = None

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
			case Some('+') => Token(TokenType.PLUS, currentCharacter.getOrElse("").toString)
			case Some('-') => Token(TokenType.MINUS, currentCharacter.getOrElse("").toString)
			case Some('*') => Token(TokenType.ASTERIX, currentCharacter.getOrElse("").toString)
			case Some('/') => Token(TokenType.SLASH, currentCharacter.getOrElse("").toString)
			case Some('<') => Token(TokenType.LT, currentCharacter.getOrElse("").toString)
			case Some('>') => Token(TokenType.GT, currentCharacter.getOrElse("").toString)
			case Some(';') => Token(TokenType.SEMICOLON, currentCharacter.getOrElse("").toString)
			case Some('(') => Token(TokenType.LPAREN, currentCharacter.getOrElse("").toString)
			case Some(')') => Token(TokenType.RPAREN, currentCharacter.getOrElse("").toString)
			case Some(',') => Token(TokenType.COMMA, currentCharacter.getOrElse("").toString)
			case Some('{') => Token(TokenType.LBRACE, currentCharacter.getOrElse("").toString)
			case Some('}') => Token(TokenType.RBRACE, currentCharacter.getOrElse("").toString)
			case Some(-1 )=> Token(TokenType.EOF, "")
			case _ => getIdentifier
		}
		readCharacter()
		token
	}

	private def getIdentifier: Token = {
		currentCharacter match {
			case Some(c) if c.toChar.isLetter =>
				val literal = readIdentifierOrNumber(_.isLetterOrDigit)
				Token(Token.lookupIdent(literal), literal)
			case Some(c) if c.toChar.isDigit =>
				val literal = readIdentifierOrNumber(c => c.isDigit || c == '.')
				if (literal.length > 1 && (literal.startsWith("0") || literal.exists(_.isLetter)))
					Token(TokenType.ILLEGAL, literal)
				else
					Token(TokenType.INT, literal)
			case Some(c) =>
				Token(TokenType.ILLEGAL, c.toString)
			case None =>
				Token(TokenType.ILLEGAL, "")
		}
	}

	private def processDoubleCharacterToken(doubleCharTokenType: TokenType, singleCharTokenType: TokenType, currentChar: Char): Token = {
		peekCharacter() match {
			case Some(c) if c == currentChar =>
				var literal = currentCharacter.getOrElse("").toString
				readCharacter()
				literal += currentCharacter.getOrElse("").toString
				Token(doubleCharTokenType, literal)
			case _ =>
				Token(singleCharTokenType, currentCharacter.getOrElse("").toString)
		}
	}

	@tailrec
	private def readIdentifierOrNumber(condition: Char => Boolean, acc: String = ""): String = {
		currentCharacter match {
			case Some(c) if condition(c.toChar) =>
				readCharacter()
				readIdentifierOrNumber(condition, acc + c.toChar)
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
