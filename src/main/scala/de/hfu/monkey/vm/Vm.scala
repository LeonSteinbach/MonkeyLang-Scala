package de.hfu.monkey.vm

import de.hfu.monkey
import de.hfu.monkey.objects.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.vm.Vm.*

val TRUE = BooleanObject(true)
val FALSE = BooleanObject(false)
val NULL = NullObject

class Vm(bytecode: Bytecode) {

	private val constants: List[Object] = bytecode.constants
	private val instructions: Instructions = bytecode.instructions
	private val stack: Array[Object] = Array.ofDim[Object](STACK_SIZE)
	private var stackPointer: Int = 0
	private val globals: Array[Object] = Array.ofDim[Object](GLOBALS_SIZE)

	def run(): Unit = {
		var ip: Int = 0
		while (ip < instructions.length) {
			val operation = instructions(ip)
			operation match {
				case OpConstant =>
					val constIndex = instructions.readInt(ip + 1)
					ip += 2
					push(constants(constIndex))
				case OpAdd | OpSub | OpMul | OpDiv =>
					executeBinaryOperation(operation)
				case OpPop =>
					pop()
				case OpTrue =>
					push(TRUE)
				case OpFalse =>
					push(FALSE)
				case OpEqual | OpNotEqual | OpGreaterThan =>
					executeComparison(operation)
				case OpBang =>
					executeBangOperator()
				case OpMinus =>
					executeMinusOperator()
				case OpJump =>
					val constIndex = instructions.readInt(ip + 1)
					ip = constIndex - 1
				case OpJumpNotTruthy =>
					val constIndex = instructions.readInt(ip + 1)
					ip += 2

					val condition = pop()
					if (!isTruthy(condition))
						ip = constIndex - 1
				case OpNull =>
					push(NULL)
				case OpSetGlobal =>
					val globalIndex = instructions.readInt(ip + 1)
					ip += 2
					globals(globalIndex) = pop()
				case OpGetGlobal =>
					val globalIndex = instructions.readInt(ip + 1)
					ip += 2
					push(globals(globalIndex))
				case OpArray =>
					val numElements = instructions.readInt(ip + 1)
					ip += 2

					val array = buildArray(stackPointer - numElements, stackPointer)
					stackPointer -= numElements

					push(array)
				case _ => throw new Exception(s"unknown operation $operation")
			}
			ip += 1
		}
	}

	private def buildArray(startIndex: Int, endIndex: Int): ArrayObject = {
		val elements = for (i <- startIndex until endIndex) yield {
			stack(i)
		}
		ArrayObject(elements.toList)
	}

	private def isTruthy(obj: Object): Boolean = {
		obj match {
			case booleanObject: BooleanObject => booleanObject.value
			case NULL => false
			case _ => true
		}
	}

	private def executeBangOperator(): Unit = {
		val operand = pop()

		operand match {
			case TRUE => push(FALSE)
			case FALSE => push(TRUE)
			case NULL => push(TRUE)
			case _ => push(FALSE)
		}
	}

	private def executeMinusOperator(): Unit = {
		val operand = pop()
		if (operand.`type`() != ObjectType.INTEGER)
			throw new Exception(s"unsupported type for negation: ${operand.`type`()}")
		push(IntegerObject(-operand.asInstanceOf[IntegerObject].value))
	}

	private def executeComparison(operation: Opcode): Unit = {
		val right = pop()
		val left = pop()

		(left, right) match {
			case (leftInt: IntegerObject, rightInt: IntegerObject) =>
				executeIntegerComparison(operation, leftInt, rightInt)
			case _ =>
				operation match {
					case OpEqual => push(BooleanObject(right == left))
					case OpNotEqual => push(BooleanObject(right != left))
					case _ => throw new Exception(s"unknown operator: $operation ${left.`type`()} ${right.`type`()}")
				}
		}
	}

	private def executeIntegerComparison(operation: Opcode, left: IntegerObject, right: IntegerObject): Unit = {
		val result = operation match {
			case OpEqual => left.value == right.value
			case OpNotEqual => left.value != right.value
			case OpGreaterThan => left.value > right.value
			case _ => throw new Exception(s"unknown operator: $operation")
		}
		push(BooleanObject(result))
	}

	private def executeBinaryOperation(operation: Opcode): Unit = {
		val right = pop()
		val left = pop()

		(left, right) match {
			case (leftInt: IntegerObject, rightInt: IntegerObject) =>
				executeBinaryIntegerOperation(operation, leftInt, rightInt)
			case (leftString: StringObject, rightString: StringObject) =>
				executeBinaryStringOperation(operation, leftString, rightString)
			case _ =>
				throw new Exception(s"unsupported types for binary operation: ${left.`type`()} ${right.`type`()}")
		}
	}

	private def executeBinaryIntegerOperation(operation: Opcode, left: IntegerObject, right: IntegerObject): Unit = {
		val result = operation match {
			case OpAdd => left.value + right.value
			case OpSub => left.value - right.value
			case OpMul => left.value * right.value
			case OpDiv => left.value / right.value
			case _ => throw new Exception(s"unknown operator: $operation")
		}
		push(IntegerObject(result))
	}

	private def executeBinaryStringOperation(operation: Opcode, left: StringObject, right: StringObject): Unit = {
		val result = operation match {
			case OpAdd => left.value + right.value
			case _ => throw new Exception(s"unknown operator: $operation")
		}
		push(StringObject(result))
	}

	private def push(obj: Object): Unit = {
		if (stackPointer >= STACK_SIZE)
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
	private val STACK_SIZE = 2048
	private val GLOBALS_SIZE = 65536
}
