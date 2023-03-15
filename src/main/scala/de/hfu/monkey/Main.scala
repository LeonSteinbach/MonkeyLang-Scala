package de.hfu.monkey

import java.io.StringReader

object Main extends App {

	/*
	private val lexer: Lexer = Lexer("; (")
	var token: Option[Token] = None
	private val startTime1 = System.currentTimeMillis()
	while (token.forall(_.tokenType != TokenType.EOF)) {
		token = Some(lexer.nextToken())
		println(token.getOrElse(Token(TokenType.ILLEGAL, "")))
	}
	private val endTime1 = System.currentTimeMillis()
	println(s"Lexer [ms]: ${endTime1 - startTime1}")
	*/

	private val lexer: Lexer = Lexer("let foo = bar; return 0;")
	private val parser: ManualParser = ManualParser(lexer)
	private val parsed: Program = parser.parseProgram()
	println("\n" + parsed.toString + "\n")
	parser.errors.foreach(error => println(error))

	/*
	private val startTime1 = System.currentTimeMillis()
	private val parser = new Parser()
	private val parsed = parser.parseAll(parser.program,
		"let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(25);")
	private val endTime1 = System.currentTimeMillis()

	private val startTime2 = System.currentTimeMillis()
	private val evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
	private val endTime2 = System.currentTimeMillis()

	println(parsed)
	println(evaluated)
	println(s"Parser [ms]:    ${endTime1 - startTime1}")
	println(s"Evaluator [ms]: ${endTime2 - startTime2}")
	*/

}
