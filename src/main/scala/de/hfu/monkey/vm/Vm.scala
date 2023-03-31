package de.hfu.monkey.vm

import de.hfu.monkey.evaluator.*
import de.hfu.monkey.compiler.Bytecode
import de.hfu.monkey.evaluator.IntegerObject

class Vm(bytecode: Bytecode) {

	def run(): Option[Exception] = {
		None
	}

	def stackTop: Object = {
		IntegerObject(123)
	}

}

object Vm {
	val stackSize = 2048
}
