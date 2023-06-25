package de.hfu.monkey.main

import de.hfu.monkey.objects.Object
import de.hfu.monkey.ast.Program
import de.hfu.monkey.code.Opcode.*
import de.hfu.monkey.code.*
import de.hfu.monkey.compiler.Compiler
import de.hfu.monkey.parser.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.vm.Vm
import scopt.OParser

import java.io.{BufferedWriter, File, FileWriter}
import scala.annotation.tailrec

case class Config(
	repl: Boolean = true,
	parser: String = "manual",
	engine: String = "interpreter",
)

object Main {
	private def fib(n: Int): Int = {
		if (n <= 1) {
			return n
		}
		fib(n - 1) + fib(n - 2)
	}

	@tailrec
	private def taiL_fib(n: Int, a: Int, b: Int): Int = {
		if (n == 0)
			return a
		else if (n == 1)
			return b
		taiL_fib(n-1, b, a+b)
	}

	def main(args: Array[String]): Unit = {
		val builder = OParser.builder[Config]
		val parser = {
			import builder.*
			OParser.sequence(
				opt[Unit]("no-repl")
					.action((_, c) => c.copy(repl = false))
					.text("Execute a predefined program instead of starting the REPL (used for testing)"),
				opt[String]("parser")
					.validate(x => if (x == "manual" || x == "combinator") success else failure("Parser must be 'manual' or 'combinator'"))
					.action((x, c) => c.copy(parser = x))
					.text("Parser implementation (manual or combinator)"),
				opt[String]("engine")
					.action((x, c) => c.copy(engine = x))
					.text("Engine implementation (interpreter or compiler)"),
			)
		}

		OParser.parse(parser, args, Config()) match {
			case Some(config) =>
				val parser = if (config.parser == "manual") ManualParser() else CombinatorParser()
				if (config.repl) {
					Repl.start(parser, config.engine)
				} else {
					println(s"Using ${config.parser} parser and ${config.engine} engine\n")
					executeProgram(parser, config.engine)
				}
			case _ =>
		}
	}

	private def executeProgram(parser: Parser, engine: String): Unit = {
		//testJvmWarmup(parser, engine)
		//compareEvaluators(parser)

		val input = "let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(30);"
		//val input = "let fib = fn(n, a, b) { if (n == 0) { return a; } else { if (n == 1) { return b; }; }; fib(n-1, b, a+b); }; fib(35, 0, 1);"

		var printString: String = s"Input:            $input\n\n"

		val startTime1 = System.currentTimeMillis()
		val parsed: Program = parser.parse(input)
		val endTime1 = System.currentTimeMillis()

		printString += s"Parsed result:    $parsed\n"
		printString += s"Parser [ms]:      ${endTime1 - startTime1}"

		var evaluated: Option[Object] = None
		val environment: Environment = new Environment
		val someParsed: Option[Program] = Some(parsed)

		val startTime2 = System.nanoTime()
		if (engine == "interpreter") {
			evaluated = Some(Evaluator.evaluate(someParsed, environment))
		} else if (engine == "compiler") {
			val compiler = Compiler()
			compiler.compile(parsed)

			//println(compiler.bytecode.instructions.inspect)
			//println(compiler.bytecode.instructions.mkString("Array(", ", ", ")"))

			val vm = Vm(compiler.bytecode)
			vm.run()

			evaluated = Some(vm.lastPoppedStackElement)
		}
		val endTime2 = System.nanoTime()
		printString += s"\n\nResult:           ${evaluated.get}\n"
		printString += s"${engine.capitalize} [ms]: ${(endTime2 - startTime2) / (1000.0 * 1000.0)}"

		println(printString)
		parser.errors.foreach(error => println(error))
	}

	private def compareEvaluators(parser: Parser): Unit = {
		val baseInput = "let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(<n>);"

		val n: Int = 0

		val input = baseInput.replace("<n>", n.toString)
		val parsed: Program = parser.parse(input)
		val someParsed: Option[Program] = Some(parsed)
		val environment: Environment = new Environment

		val startInterpreter = System.nanoTime()
		Evaluator.evaluate(someParsed, environment)
		val endInterpreter = System.nanoTime()

		val startCompiler = System.nanoTime()
		val compiler = Compiler()
		compiler.compile(parsed)
		val vm = Vm(compiler.bytecode)
		val endCompiler = System.nanoTime()
		vm.run()

		println(s"${(endInterpreter - startInterpreter) / (1000.0 * 1000.0)}\t${(endCompiler - startCompiler) / (1000.0 * 1000.0)}")
	}

	private def testJvmWarmup(parser: Parser, engine: String): Unit = {
		//val input = "let fib = fn(n, a, b) { if (n == 0) { return a; } else { if (n == 1) { return b; }; }; fib(n-1, b, a+b); }; fib(35, 0, 1);"
		val input = "let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(35);"

		val parsed: Program = parser.parse(input)
		var results = Array[Double]()
		val string = new StringBuilder("")

		for (i <- 0 to 10000) {
			var evaluated: Option[Object] = None

			var a = System.nanoTime()
			var b = System.nanoTime()

			var end = System.nanoTime()
			var start = System.nanoTime()

			if (engine == "interpreter") {
				val env = new Environment
				start = System.nanoTime()
				evaluated = Some(Evaluator.evaluate(Some(parsed), env))
				end = System.nanoTime()
			} else if (engine == "compiler") {
				val compiler = Compiler()

				a = System.nanoTime()
				compiler.compile(parsed)
				b = System.nanoTime()

				val vm = Vm(compiler.bytecode)

				start = System.nanoTime()
				vm.run()
				end = System.nanoTime()

				evaluated = Some(vm.lastPoppedStackElement)
			}

			val time = (end - start) / (1000.0 * 1000.0)

			results = results :+ time
			string.append(time + "\n")
			println(time)
		}

		val avg = results.sum / results.length

		println(avg)
		println(results.last)
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

	private def concatInstructions(instructions: List[Instructions]): Instructions = {
		var out: Instructions = Array.empty[UnsignedByte]
		for (ins <- instructions) {
			out = Array.concat(out, ins)
		}
		out
	}

	private def writeFile(filename: String, s: String): Unit = {
		val file = new File(filename)
		val bufferedWriter = new BufferedWriter(new FileWriter(file))
		bufferedWriter.write(s)
		bufferedWriter.close()
	}
}
