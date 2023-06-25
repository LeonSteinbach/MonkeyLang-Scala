package de.hfu.monkey.main

import de.hfu.monkey.compiler.*
import de.hfu.monkey.vm.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.objects.Object
import de.hfu.monkey.parser.Parser

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

object Repl {
	private val face =
		"""
                __,__
       .--.  .-"     "-.  .--.
      / .. \/  .-. .-.  \/ .. \
     | |  '|  /   Y   \  |'  | |
     | \   \  \ 0 | 0 /  /   / |
      \ '- ,\.---------./, -' /
       ''-' /_   ^ ^   _\ '-''
           |  \._   _./  |
           \   \ '~' /   /
            '._ '-=-' _.'
               '-----'
	"""

	def start(parser: Parser, engine: String): Unit = {

		println(face)
		println("Welcome to the monkey REPL.\n\nType 'exit' to terminate the REPL.\nType 'help' for more options.\n")

		val environment = new Environment
		val symbolTable = new SymbolTable
		val constants = ArrayBuffer[Object]()
		val globals = Array.ofDim[Object](65536)

		while (true) {
			val input = readLine(">>> ")

			input.trim() match {
				case "exit" =>
					println("terminating REPL")
					return
				case "help" =>
					println(
						"""Usage:  [options]
						  |
						  |  --no-repl         Execute a predefined program instead of starting the REPL (used for testing)
						  |  --parser <value>  Parser implementation (manual or combinator)
						  |  --engine <value>  Engine implementation (interpreter or compiler)
						  |""".stripMargin + "\n")
				case _ =>
					execute(input, parser, engine, environment, symbolTable, constants, globals)
			}
		}
	}

	private def execute(input: String, parser: Parser, engine: String, environment: Environment, symbolTable: SymbolTable, constants: ArrayBuffer[Object], globals: Array[Object]): Unit = {
		val program = parser.parse(input)
		var evaluated: Option[Object] = None

		var start = System.nanoTime()
		if (engine == "interpreter") {
			start = System.nanoTime()
			evaluated = Some(Evaluator.evaluate(Some(program), environment))
		} else if (engine == "compiler") {
			val compiler = Compiler(constants.toArray, symbolTable)
			compiler.compile(program)
			val bytecode = compiler.bytecode
			constants.clear()
			constants ++= bytecode.constants
			val vm = Vm(bytecode, globals)
			start = System.nanoTime()
			vm.run()

			evaluated = Some(vm.lastPoppedStackElement)
		}
		val end = System.nanoTime()

		println(evaluated.get.toString)
		//println(s"Finished in ${(end - start) / (1000.0 * 1000.0)} ms")
	}
}
