package de.hfu.monkey

import java.io.StringReader

object Main extends App {

/*
	private val lexer: Lexer = Lexer("if (true) { 1; } else { 2; };")
	var token: Option[Token] = None
	private val startTime1 = System.currentTimeMillis()
	while (token.forall(_.tokenType != TokenType.EOF)) {
		token = Some(lexer.nextToken())
		println(token.getOrElse(Token(TokenType.ILLEGAL, "")))
	}
	private val endTime1 = System.currentTimeMillis()
	println(s"Lexer [ms]: ${endTime1 - startTime1}")
*/

	private val input: String = "let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(25);"

	private val lexer: Lexer = Lexer(input)
	private val manualParser: ManualParser = ManualParser(lexer)

	private val startTime1 = System.currentTimeMillis()
	private var parsed: Program = manualParser.parseProgram()
	private val endTime1 = System.currentTimeMillis()

	private val combinatorParser = new CombinatorParser()
	private val startTime2 = System.currentTimeMillis()
	parsed = combinatorParser.parseAll(combinatorParser.program, input).get
	private val endTime2 = System.currentTimeMillis()

	private val startTime3 = System.currentTimeMillis()
	private val evaluated = Evaluator.evaluateProgram(parsed, new Environment)
	private val endTime3 = System.currentTimeMillis()

	println(parsed)
	println(evaluated)
	println(s"Manual Parser [ms]:     ${endTime1 - startTime1}")
	println(s"Combinator Parser [ms]: ${endTime2 - startTime2}")
	println(s"Evaluator [ms]:         ${endTime3 - startTime3}")

	manualParser.errors.foreach(error => println(error))

}
