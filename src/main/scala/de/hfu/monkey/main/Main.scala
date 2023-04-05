package de.hfu.monkey.main

import de.hfu.monkey.objects.Object
import de.hfu.monkey.ast.Program
import de.hfu.monkey.compiler.Compiler
import de.hfu.monkey.parser.{CombinatorParser, ManualParser, Parser}
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.vm.Vm
import scopt.OParser

import java.io.{BufferedWriter, File, FileWriter}

case class Config(
	parser: String = "manual",
	engine: String = "interpreter",
	evaluate: Boolean = true,
	compareParsers: Boolean = false,
	appendMode: String = "linear",
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
				opt[String]("engine")
					.action((x, c) => c.copy(engine = x))
					.text("Engine implementation (interpreter or compiler)"),
				opt[Unit]("compareParsers")
					.action((_, c) => c.copy(compareParsers = true))
					.text("Compare the different parser implementations by timing them")
					.children(
						opt[Int]("iterations")
							.action((x, c) => c.copy(iterations = x))
							.text("Number of iterations (length of the parsed text)"),
						opt[Int]("steps")
							.action((x, c) => c.copy(steps = x))
							.text("Number of steps to take"),
						opt[String]("file-parser")
							.action((x, c) => c.copy(timingsParserFilename = Some(x)))
							.text("File to write parser timings comparison"),
						opt[String]("appendMode")
							.action((x, c) => c.copy(appendMode = x))
							.validate(x => if (x == "linear" || x == "nested") success else failure("Append mode must be 'linear' or 'nested'"))
							.text("Append mode for building parsers (linear or nested)")
					)
			)
		}

		OParser.parse(parser, args, Config()) match {
			case Some(config) =>
				val parser = if (config.parser == "manual") ManualParser() else CombinatorParser()
				if (config.compareParsers) {
					compareParsers(config.iterations, config.steps, config.appendMode, config.timingsParserFilename)
				} else {
					println(s"Using ${config.parser} parser and ${config.engine}\n")
					printResult(parser, config.engine, config.evaluate)
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

	private def printResult(parser: Parser, engine: String, evaluate: Boolean): Unit = {
		val input = "let a = {1: 2, 3: 4}; a;"
		var printString: String = ""

		val startTime1 = System.currentTimeMillis()
		val parsed: Program = parser.parse(input)
		val endTime1 = System.currentTimeMillis()
		printString += s"Parsed result:    $parsed\n"
		printString += s"Parser [ms]:      ${endTime1 - startTime1}"

		if (evaluate) {
			var evaluated: Option[Object] = None
			var startTime2 = System.currentTimeMillis()
			if (engine == "interpreter") {
				evaluated = Some(Evaluator.evaluateProgram(parsed, new Environment))
			} else if (engine == "compiler") {
				val compiler = Compiler()
				compiler.compile(parsed)

				val vm = Vm(compiler.bytecode)
				startTime2 = System.currentTimeMillis()
				vm.run()

				evaluated = Some(vm.lastPoppedStackElement)
			}
			val endTime2 = System.currentTimeMillis()
			printString += s"\n\n${engine.capitalize} [ms]:   ${endTime2 - startTime2}\n"
			printString += s"Result: ${evaluated.get}"
		}

		println(printString)

		parser.errors.foreach(error => println(error))
	}

	private def compareParsers(iterations: Int, steps: Int, appendMode: String, filename: Option[String]): Unit = {

		val parserInput: String =
			if (appendMode == "nested")
				"foo + bar("
			else
				"let foo = fn(a, b) { let bar = a + b; return bar * bar; }; let x = foo(foo(1, 2), 3); if (-x > 0 == true == !false) { (0 + 1) * 2; } else { x; };"

		val printString = new StringBuilder("# Iterations\tManual\tCombinators\n")

		val parserManual: Parser = ManualParser()
		val parserCombinator: Parser = CombinatorParser()

		for (multiply <- 0.to(iterations, steps)) {
			val finalInput = new StringBuilder("")
			if (appendMode == "nested") {
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
