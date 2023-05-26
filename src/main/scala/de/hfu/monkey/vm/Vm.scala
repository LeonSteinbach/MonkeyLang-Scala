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

class Vm(bytecode: Bytecode, private var globals: Array[Object] = Array.ofDim[Object](GLOBALS_SIZE)) {

	private val constants: List[Object] = bytecode.constants
	private val stack: Array[Object] = Array.ofDim[Object](STACK_SIZE)
	private var stackPointer: Int = 0
	private val frames: Array[Frame] = Array.ofDim[Frame](MAX_FRAMES)
	private val mainClosure: ClosureObject = ClosureObject(CompiledFunctionObject(bytecode.instructions))
	frames(0) = Frame(mainClosure, 0)
	private var framesIndex: Int = 1

	def run(): Unit = {
		var ip: Int = 0
		var ins: Instructions = Array.empty
		var operation: Opcode = 0.toUnsignedByte

		while (currentFrame.ip < currentFrame.instructions.length - 1) {
			currentFrame.ip += 1
			ip = currentFrame.ip
			ins = currentFrame.instructions

			operation = ins(ip)
			operation match {
				case OpConstant =>
					val constIndex = ins.readInt(ip + 1)
					currentFrame.ip += 2
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
					val constIndex = ins.readInt(ip + 1)
					currentFrame.ip = constIndex - 1
				case OpJumpNotTruthy =>
					val constIndex = ins.readInt(ip + 1)
					currentFrame.ip += 2

					val condition = pop()
					if (!isTruthy(condition))
						currentFrame.ip = constIndex - 1
				case OpNull =>
					push(NULL)
				case OpSetGlobal =>
					val globalIndex = ins.readInt(ip + 1)
					currentFrame.ip += 2
					globals(globalIndex) = pop()
				case OpGetGlobal =>
					val globalIndex = ins.readInt(ip + 1)
					currentFrame.ip += 2
					push(globals(globalIndex))
				case OpSetLocal =>
					val localIndex = ins.readByte(ip + 1)
					currentFrame.ip += 1
					stack(currentFrame.basePointer + localIndex.toInt) = pop()
				case OpGetLocal =>
					val localIndex = ins.readByte(ip + 1)
					currentFrame.ip += 1
					push(stack(currentFrame.basePointer + localIndex.toInt))
				case OpArray =>
					val numElements = ins.readInt(ip + 1)
					currentFrame.ip += 2

					val array = buildArray(stackPointer - numElements, stackPointer)
					stackPointer -= numElements

					push(array)
				case OpHash =>
					val numElements = ins.readInt(ip + 1)
					currentFrame.ip += 2

					val hash = buildHash(stackPointer - numElements, stackPointer - 1)
					stackPointer -= numElements

					push(hash)
				case OpIndex =>
					val index = pop()
					val left = pop()
					executeIndexExpression(left, index)
				case OpCall =>
					val numArgs = ins.readByte(ip + 1)
					currentFrame.ip += 1
					executeCall(numArgs.toInt)
				case OpReturnValue =>
					val returnValue = pop()
					val frame = popFrame()
					stackPointer = frame.basePointer - 1
					push(returnValue)
				case OpReturn =>
					val frame = popFrame()
					stackPointer = frame.basePointer - 1
					push(NULL)
				case OpClosure =>
					val constIndex = ins.readInt(ip + 1)
					val numFree = ins.readByte(ip + 3)
					currentFrame.ip += 3
					pushClosure(constIndex, numFree.toInt)
				case OpGetFree =>
					val freeIndex = ins.readByte(ip + 1)
					currentFrame.ip += 1
					push(currentFrame.closure.free(freeIndex.toInt))
				case OpCurrentClosure =>
					push(currentFrame.closure)
				case _ => throw new Exception(s"unknown operation $operation")
			}
		}
	}

	private def pushClosure(constIndex: Int, numFree: Int): Unit = {
		val function = constants(constIndex) match {
			case compiledFunctionObject: CompiledFunctionObject => compiledFunctionObject
			case _ => throw new Exception(s"closure is not a function")
		}
		val free = stack.slice(stackPointer - numFree, stackPointer).toList
		stackPointer -= numFree
		push(ClosureObject(function, free))
	}

	private def executeCall(numArgs: Int): Unit = {
		stack(stackPointer - 1 - numArgs) match {
			case closure: ClosureObject => callClosure(closure, numArgs)
			case builtin: BuiltinObject => // TODO: Implement builtins call
			case _ => throw new Exception("calling non-closure and non-builtin")
		}
	}

	private def callClosure(closure: ClosureObject, numArgs: Int): Unit = {
		if (numArgs != closure.function.numParameters)
			throw new Exception(s"wrong number of arguments: want=${closure.function.numParameters}, got=$numArgs")
		val frame = Frame(closure, stackPointer - numArgs)
		pushFrame(frame)
		stackPointer = frame.basePointer + closure.function.numLocals
	}

	private def currentFrame: Frame = frames(framesIndex - 1)

	private def pushFrame(frame: Frame): Unit = {
		frames(framesIndex) = frame
		framesIndex += 1
	}

	private def popFrame(): Frame = {
		framesIndex -= 1
		frames(framesIndex)
	}

	private def buildArray(startIndex: Int, endIndex: Int): ArrayObject = {
		val elements = for (i <- startIndex until endIndex) yield {
			stack(i)
		}
		ArrayObject(elements.toList)
	}

	private def buildHash(startIndex: Int, endIndex: Int): HashObject = {
		val elements = for (i <- startIndex until endIndex by 2) yield {
			val key = stack(i).asInstanceOf[Hashable]
			val value = stack(i + 1)
			key.hashKey -> HashPair(key, value)
		}
		HashObject(elements.toMap)
	}

	private def isTruthy(obj: Object): Boolean = {
		obj match {
			case booleanObject: BooleanObject => booleanObject.value
			case NULL => false
			case _ => true
		}
	}

	private def executeIndexExpression(left: Object, index: Object): Unit = {
		if (left.`type`() == ObjectType.ARRAY && index.`type`() == ObjectType.INTEGER)
			executeArrayIndex(left.asInstanceOf[ArrayObject], index.asInstanceOf[IntegerObject])
		else if (left.`type`() == ObjectType.HASH)
			executeHashIndex(left.asInstanceOf[HashObject], index)
		else
			throw new Exception(s"index operator not supported: ${left.`type`()}")
	}

	private def executeArrayIndex(left: ArrayObject, index: IntegerObject): Unit = {
		if (index.value < 0 || index.value > left.elements.length - 1)
			push(NULL)
		else
			push(left.elements(index.value))
	}

	private def executeHashIndex(left: HashObject, index: Object): Unit = {
		val key: Hashable = index match {
			case hashable: Hashable => hashable
			case _ => throw new Exception(s"index unusable as hash key: ${index.`type`()}")
		}
		left.pairs.get(key.hashKey) match {
			case Some(pair: HashPair) => push(pair.value)
			case None => push(NULL)
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
	private val MAX_FRAMES = 1024
}
