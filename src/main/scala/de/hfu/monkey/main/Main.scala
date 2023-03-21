package de.hfu.monkey.main

import de.hfu.monkey.Ast.Program
import de.hfu.monkey.Parser.{Parser, CombinatorParser, ManualParser}
import de.hfu.monkey.evaluator.*
import scopt.OParser

import java.io.{BufferedWriter, File, FileWriter}

case class Config(
	parser: String = "manual",
	evaluator: String = "interpreter",
	evaluate: Boolean = true,
	compareParsers: Boolean = false,
	nested: Boolean = false,
	iterations: Int = 10000,
	steps: Int = 10,
	timingsParserFilename: Option[String] = None
)

object Main {
	def main(args: Array[String]): Unit = {

		val builder = OParser.builder[Config]
		val parser = {
			import builder.*
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
							.text("File to write parser timings comparison"),
						opt[Unit]("nested")
							.action((_, c) => c.copy(nested = true))
							.text("Use nested function calls instead of linear append")
					)
			)
		}

		OParser.parse(parser, args, Config()) match {
			case Some(config) =>
				val parser = if (config.parser == "manual") ManualParser() else CombinatorParser()
				if (config.compareParsers) {
					compareParsers(config.iterations, config.steps, config.nested, config.timingsParserFilename)
				} else {
					println(s"Using ${config.parser} parser\n")
					printResult(parser, config.evaluator, config.evaluate)
				}
			case _ =>
		}

/*
		val lexer: Lexer = Lexer("\"hallo\";")
		var token: Option[Token] = None
		val startTime1 = System.currentTimeMillis()
		while (token.forall(_.tokenType != TokenType.EOF)) {
			token = Some(lexer.nextToken())
			println(token.getOrElse(Token(TokenType.ILLEGAL, "")))
		}
		val endTime1 = System.currentTimeMillis()
		println(s"Lexer [ms]: ${endTime1 - startTime1}")
*/
	}

	private def printResult(parser: Parser, evaluator: String, evaluate: Boolean): Unit = {
		val input = "let a = {\"0\": 1, 1: 2, 2 + 3: 3, true: 6}; a[5];"
		var printString: String = ""

		val startTime1 = System.currentTimeMillis()
		val parsed: Program = parser.parse(input)
		val endTime1 = System.currentTimeMillis()
		printString += s"Parsed result:    $parsed\n"
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

	private def compareParsers(iterations: Int, steps: Int, nested: Boolean, filename: Option[String]): Unit = {

		val parserInput: String = if (nested) "foo + bar(" else "let foo = fn(a, b) { let bar = a + b; return bar * bar; }; let x = foo(foo(1, 2), 3); if (-x > 0 == true == !false) { (0 + 1) * 2; } else { x; };"

		val printString = new StringBuilder("# Iterations\tManual\tCombinators\n")

		val parserManual: Parser = ManualParser()
		val parserCombinator: Parser = CombinatorParser()

		for (multiply <- 0.to(iterations, steps)) {
			val finalInput = new StringBuilder("")
			if (nested) {
				finalInput.append(parserInput * multiply + ")" * multiply + (if (multiply > 0) ";" else ""))
			} else {
				finalInput.append(parserInput * multiply)
			}

			val startTime1 = System.currentTimeMillis()
			parserManual.parse(finalInput.toString())
			val endTime1 = System.currentTimeMillis()

			val startTime2 = System.currentTimeMillis()
			parserCombinator.parse(finalInput.toString())
			val endTime2 = System.currentTimeMillis()

			printString.append(s"$multiply\t${endTime1 - startTime1}\t${endTime2 - startTime2}\n")
			print(s"[$multiply/$iterations]\r")
		}

		filename match {
			case Some(filename: String) =>
				val finalString = printString.toString()
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
