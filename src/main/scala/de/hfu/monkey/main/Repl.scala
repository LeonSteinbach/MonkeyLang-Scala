package de.hfu.monkey.main

import de.hfu.monkey.compiler.*
import de.hfu.monkey.vm.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.objects.Object
import de.hfu.monkey.parser.Parser

import java.util.Scanner
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
		val environment = new Environment
		var constants = Array[Object]()
		val symbolTable = new SymbolTable()
		val globals = Array.ofDim[Object](65536)

		println(face)
		println("Welcome to the monkey REPL")

		while (true) {
			val input = readLine(">>> ")

			if (input.trim() == "exit") {
				println("terminating REPL")
				return
			}

			val program = parser.parse(input)
			var evaluated: Option[Object] = None

			var start = System.nanoTime()
			if (engine == "interpreter") {
				start = System.nanoTime()
				evaluated = Some(Evaluator.evaluateProgram(program, environment))
			} else if (engine == "compiler") {
				val compiler = Compiler(constants, symbolTable)
				compiler.compile(program)
				val bytecode = compiler.bytecode
				constants = bytecode.constants.toArray
				val vm = Vm(bytecode, globals)
				start = System.nanoTime()
				vm.run()

				evaluated = Some(vm.lastPoppedStackElement)
			}
			val end = System.nanoTime()

			println(evaluated.get.toString)
			println(s"Finished in ${(end - start) / (1000.0 * 1000.0)} ms")
		}
	}
}
