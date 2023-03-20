package de.hfu.monkey

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Failure

class LexerTest extends AnyFunSuite {

	test("lexer.keyword") {
		val lexer: Lexer = Lexer("fn let true false if else return")

		assert(lexer.nextToken() === Token(TokenType.FUNCTION, "fn"))
		assert(lexer.nextToken() === Token(TokenType.LET, "let"))
		assert(lexer.nextToken() === Token(TokenType.TRUE, "true"))
		assert(lexer.nextToken() === Token(TokenType.FALSE, "false"))
		assert(lexer.nextToken() === Token(TokenType.IF, "if"))
		assert(lexer.nextToken() === Token(TokenType.ELSE, "else"))
		assert(lexer.nextToken() === Token(TokenType.RETURN, "return"))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

	test("lexer.identifier") {
		val lexer: Lexer = Lexer("foo bar123 foo123bar")

		assert(lexer.nextToken() === Token(TokenType.IDENT, "foo"))
		assert(lexer.nextToken() === Token(TokenType.IDENT, "bar123"))
		assert(lexer.nextToken() === Token(TokenType.IDENT, "foo123bar"))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

	test("lexer.integer") {
		val lexer: Lexer = Lexer("0 1 123 01 00")

		assert(lexer.nextToken() === Token(TokenType.INT, "0"))
		assert(lexer.nextToken() === Token(TokenType.INT, "1"))
		assert(lexer.nextToken() === Token(TokenType.INT, "123"))
		assert(lexer.nextToken() === Token(TokenType.ILLEGAL, "01"))
		assert(lexer.nextToken() === Token(TokenType.ILLEGAL, "00"))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

	test("lexer.string") {
		val lexer: Lexer = Lexer("\"hello\"")

		assert(lexer.nextToken() === Token(TokenType.STRING, "hello"))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

	test("lexer.singleCharacter") {
		val lexer: Lexer = Lexer(";(){},=!+-*/<>")

		assert(lexer.nextToken() === Token(TokenType.SEMICOLON, ";"))
		assert(lexer.nextToken() === Token(TokenType.LPAREN, "("))
		assert(lexer.nextToken() === Token(TokenType.RPAREN, ")"))
		assert(lexer.nextToken() === Token(TokenType.LBRACE, "{"))
		assert(lexer.nextToken() === Token(TokenType.RBRACE, "}"))
		assert(lexer.nextToken() === Token(TokenType.COMMA, ","))
		assert(lexer.nextToken() === Token(TokenType.ASSIGN, "="))
		assert(lexer.nextToken() === Token(TokenType.BANG, "!"))
		assert(lexer.nextToken() === Token(TokenType.PLUS, "+"))
		assert(lexer.nextToken() === Token(TokenType.MINUS, "-"))
		assert(lexer.nextToken() === Token(TokenType.ASTERIX, "*"))
		assert(lexer.nextToken() === Token(TokenType.SLASH, "/"))
		assert(lexer.nextToken() === Token(TokenType.LT, "<"))
		assert(lexer.nextToken() === Token(TokenType.GT, ">"))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

	test("lexer.doubleCharacter") {
		val lexer: Lexer = Lexer("== !=")

		assert(lexer.nextToken() === Token(TokenType.EQ, "=="))
		assert(lexer.nextToken() === Token(TokenType.NEQ, "!="))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

	test("lexer.illegal") {
		val lexer: Lexer = Lexer("#$%&")

		assert(lexer.nextToken() === Token(TokenType.ILLEGAL, "#"))
		assert(lexer.nextToken() === Token(TokenType.ILLEGAL, "$"))
		assert(lexer.nextToken() === Token(TokenType.ILLEGAL, "%"))
		assert(lexer.nextToken() === Token(TokenType.ILLEGAL, "&"))
		assert(lexer.nextToken() === Token(TokenType.EOF, ""))
	}

}
