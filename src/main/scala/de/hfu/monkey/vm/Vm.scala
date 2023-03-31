package de.hfu.monkey.vm

import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.Opcode
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.vm.Vm.*

class Vm(bytecode: Bytecode) {

	private val constants: List[Object] = bytecode.constants
	private val instructions: Instructions = bytecode.instructions
	private val stack: Array[Object] = Array.ofDim[Object](stackSize)
	private var stackPointer: Int = 0

	def run(): Option[Exception] = {
		var ip: Int = 0
		while (ip < instructions.length - 1) {
			instructions(ip) match {
				case Opcode.OpConstant =>
					val constIndex = instructions.readInt(ip + 1)
					ip += 2

					push(constants(constIndex)) match {
						case Some(exception: Exception) => return Some(exception)
						case None =>
					}
			}
		}
		ip += 1
		None
	}

	private def push(obj: Object): Option[Exception] = {
		if (stackPointer >= stackSize)
			Some(new Exception("stack overflow"))
		else {
			stack(stackPointer) = obj
			stackPointer += 1
			None
		}
	}

	def stackTop: Option[Object] = if (stackPointer == 0) None else Some(stack.head)

}

object Vm {
	private val stackSize = 2048
}
