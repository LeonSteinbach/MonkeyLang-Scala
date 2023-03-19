package de.hfu.monkey

import de.hfu.monkey.Parser.{CombinatorParser, ManualParser}
import scopt.OParser

import java.io.{BufferedWriter, File, FileWriter}

case class Config(
	parser: String = "manual",
	evaluator: String = "interpreter",
	evaluate: Boolean = true,
	compareParsers: Boolean = false,
	iterations: Int = 10000,
	steps: Int = 100,
	timingsParserFilename: Option[String] = None
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
				opt[Int]("iterations")
					.action((x, c) => c.copy(iterations = x))
					.text("Number of iterations (length of the parsed text)"),
				opt[Int]("steps")
					.action((x, c) => c.copy(steps = x))
					.text("Number of steps to take"),
				opt[Unit]("compareParsers")
					.action((_, c) => c.copy(compareParsers = true))
					.text("Compare the different parser implementations by timing them")
					.children(
						opt[String]("file-parser")
							.action((x, c) => c.copy(timingsParserFilename = Some(x)))
							.text("File to write parser timings comparison")
					)
			)
		}

		OParser.parse(parser, args, Config()) match {
			case Some(config) =>
				val parser = if (config.parser == "manual") ManualParser() else CombinatorParser()
				if (config.compareParsers) {
					compareParsers(config.iterations, config.steps, config.timingsParserFilename)
				} else {
					println(s"Using ${config.parser} parser\n")
					printResult(parser, config.evaluator, config.evaluate)
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

	private def printResult(parser: Parser.Parser, evaluator: String, evaluate: Boolean): Unit = {
		val input = "let foo = fn(a, b) { let bar = a + b; return bar * bar; }; let x = foo(foo(1, 2), 3); if (-x > 0 == true == !false) { (0 + 1) * 2; } else { x; };"
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

	private def compareParsers(iterations: Int, steps: Int, filename: Option[String]): Unit = {

		val out = new java.io.PrintStream(System.out, true)

		val parserInput: String = "let foo = fn(a, b) { let bar = a + b; return bar * bar; }; let x = foo(foo(1, 2), 3); if (-x > 0 == true == !false) { (0 + 1) * 2; } else { x; };"

		val printStringManual = new StringBuilder("# Iterations Duration\n# First data block (index 0)\n")
		val printStringCombinators = new StringBuilder("\n\n# Iterations Duration\n# Second data block (index 1)\n")

		val parserManual: Parser.Parser = Parser.ManualParser()
		val parserCombinator: Parser.Parser = Parser.CombinatorParser()

		for (multiply <- 0.to(iterations, steps)) {
			val startTime1 = System.currentTimeMillis()
			parserManual.parse(parserInput * multiply)
			val endTime1 = System.currentTimeMillis()

			val startTime2 = System.currentTimeMillis()
			parserCombinator.parse(parserInput * multiply)
			val endTime2 = System.currentTimeMillis()

			printStringManual.append(s"$multiply ${endTime1 - startTime1}\n")
			//printStringCombinators.append(s"$multiply 1\n")
			printStringCombinators.append(s"$multiply ${endTime2 - startTime2}\n")
			out.println(s"[$multiply/$iterations]")
		}

		println(printStringManual.toString() + "\n\n")
		println(printStringCombinators.toString())

		filename match {
			case Some(filename: String) =>
				val finalString = printStringManual.toString() + printStringCombinators.toString()
				writeFile(Some(filename).get, finalString)
			case _ =>
		}

	}

	private def writeFile(filename: String, s: String): Unit = {
		val file = new File(filename)
		val bufferedWriter = new BufferedWriter(new FileWriter(file))
		bufferedWriter.write(s)
		bufferedWriter.close()
	}
}
