package de.hfu.monkey.vm

import de.hfu.monkey
import de.hfu.monkey.objects.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.vm.Vm.*

class Vm(bytecode: Bytecode) {

	private val constants: List[Object] = bytecode.constants
	private val instructions: Instructions = bytecode.instructions
	private val stack: Array[Object] = Array.ofDim[Object](stackSize)
	private var stackPointer: Int = 0

	def run(): Unit = {
		var ip: Int = 0
		while (ip < instructions.length) {
			val operation = instructions(ip)
			operation match {
				case OpConstant =>
					val constIndex = instructions.readInt(ip + 1)
					ip += 2
					push(constants(constIndex))
				case OpAdd =>
					val right = pop().asInstanceOf[IntegerObject]
					val left = pop().asInstanceOf[IntegerObject]
					push(IntegerObject(left.value + right.value))
				case OpPop =>
					pop()
				case _ => throw new Exception(s"unknown operation $operation")
			}
			ip += 1
		}
	}

	private def push(obj: Object): Unit = {
		if (stackPointer >= stackSize)
			throw new Exception("stack overflow")
		else {
			stack(stackPointer) = obj
			stackPointer += 1
		}
	}

	private def pop(): Object = {
		val obj: Object = stack(stackPointer - 1)
		stackPointer -= 1
		obj
	}

	def lastPoppedStackElement: Object = stack(stackPointer)

}

object Vm {
	private val stackSize = 2048
}
