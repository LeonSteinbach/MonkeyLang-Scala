package de.hfu.monkey

import de.hfu.monkey.Parser.{CombinatorParser, ManualParser}
import scopt.OParser

case class Config(
	parser: String = "manual",
	evaluator: String = "interpreter",
	evaluate: Boolean = true,
	compare: Boolean = false
)

object Main {
	def main(args: Array[String]): Unit = {

		val builder = OParser.builder[Config]
		val parser = {
			import builder._
			OParser.sequence(
				opt[String]("parser")
					.validate(x => if (x == "manual" || x == "combinator") success else failure("Parser must be 'manual' or 'combinator'"))
					.action((x, c) => c.copy(parser = x))
					.text("Parser implementation (manual or combinator)"),
				opt[String]("evaluator")
					.action((x, c) => c.copy(evaluator = x))
					.text("Evaluator implementation (interpreter or compiler)"),
				opt[Unit]("no-evaluate")
					.action((_, c) => c.copy(evaluate = false))
					.text("Do not evaluate the input program"),
				opt[Unit]("compare")
					.action((_, c) => c.copy(compare = true))
					.text("Compare the different implementations by timing them")
			)
		}

		OParser.parse(parser, args, Config()) match {
			case Some(config) =>
				val parser = if (config.parser == "manual") ManualParser() else CombinatorParser()
				val input = "let foo = fn(a, b) { return a + b * 2; }; foo(4, 5);" * 10000
				if (config.compare) {
					// Compare timings
				} else {
					println(s"Using ${config.parser} parser\n")
					printResult(input, parser, config.evaluator, config.evaluate)
				}
			case _ =>
		}

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
	}

	private def printResult(input: String, parser: Parser.Parser, evaluator: String, evaluate: Boolean): Unit = {
		var printString: String = ""

		val startTime1 = System.currentTimeMillis()
		val parsed: Program = parser.parse(input)
		val endTime1 = System.currentTimeMillis()
		printString += s"Parser [ms]:      ${endTime1 - startTime1}"

		if (evaluate) {
			val startTime2 = System.currentTimeMillis()
			var evaluated: Option[Object] = None
			if (evaluator == "interpreter") {
				evaluated = Some(Evaluator.evaluateProgram(parsed, new Environment))
			} else if (evaluator == "compiler") {
				// TODO: Implement compiler
			}
			val endTime2 = System.currentTimeMillis()
			printString += s"\n\nEvaluator [ms]:   ${endTime2 - startTime2}\n"
			printString += s"Evaluated result: ${evaluated.get}"
		}

		println(printString)

		parser.errors.foreach(error => println(error))
	}
}
