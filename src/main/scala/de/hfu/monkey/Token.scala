package de.hfu.monkey

enum TokenType {
	case ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, MINUS, BANG, ASTERIX, SLASH, LT, GT, COMMA, COLON, SEMICOLON,
	LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET, STRING, TRUE, FALSE, IF, ELSE, RETURN, EQ, NEQ, FUNCTION, LET
}

case class Token(tokenType: TokenType, literal: String) {
	override def toString: String = s"{ Type: $tokenType, Literal: $literal }"
}

object Token {
	private val keywords: Map[String, TokenType] = Map(
		"fn" -> TokenType.FUNCTION,
		"let" -> TokenType.LET,
		"true" -> TokenType.TRUE,
		"false" -> TokenType.FALSE,
		"if" -> TokenType.IF,
		"else" -> TokenType.ELSE,
		"return" -> TokenType.RETURN
	)

	def lookupIdent(ident: String): TokenType = keywords.getOrElse(ident, TokenType.IDENT)
}
