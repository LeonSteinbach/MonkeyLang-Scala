package de.hfu.monkey.compiler

import de.hfu.monkey.ast.*
import de.hfu.monkey.objects.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*

case class Compiler() {
	var instructions: Instructions = Array[Byte]()
	var constants: Array[Object] = Array[Object]()

	private val symbolTable: SymbolTable = new SymbolTable()

	private var lastInstruction: Option[EmittedInstruction] = None
	private var previousInstruction: Option[EmittedInstruction] = None

	def compile(node: Node): Unit = {
		node match {
			case program: Program =>
				program.statements.foreach {
					statement => compile(statement)
				}
			case expressionStatement: ExpressionStatement =>
				compile(expressionStatement.expression)
				emit(OpPop)
			case infixExpression: InfixExpression =>
				if (infixExpression.operator == "<") {
					compile(infixExpression.right)
					compile(infixExpression.left)
					emit(OpGreaterThan)
				}
				else {
					compile(infixExpression.left)
					compile(infixExpression.right)

					infixExpression.operator match {
						case "+" => emit(OpAdd)
						case "-" => emit(OpSub)
						case "*" => emit(OpMul)
						case "/" => emit(OpDiv)
						case ">" => emit(OpGreaterThan)
						case "==" => emit(OpEqual)
						case "!=" => emit(OpNotEqual)
						case operator => throw new Exception(s"unknown operator $operator")
					}
				}
			case prefixExpression: PrefixExpression =>
				compile(prefixExpression.value)

				prefixExpression.operator match {
					case "!" => emit(OpBang)
					case "-" => emit(OpMinus)
					case operator => throw new Exception(s"unknown operator $operator")
				}
			case ifExpression: IfExpression =>
				compile(ifExpression.condition)
				val jumpNotTruthyPosition = emit(OpJumpNotTruthy, 9999)
				compile(ifExpression.consequence)
				if (lastInstructionIsPop)
					removeLastPop()

				val jumpPosition = emit(OpJump, 9999)
				val afterConsequencePosition = instructions.length
				changeOperand(jumpNotTruthyPosition, afterConsequencePosition)

				if (ifExpression.alternative.statements.isEmpty) {
					emit(OpNull)
				} else {
					compile(ifExpression.alternative)
					if (lastInstructionIsPop)
						removeLastPop()
				}
				val afterAlternativePosition = instructions.length
				changeOperand(jumpPosition, afterAlternativePosition)
			case blockStatement: BlockStatement =>
				blockStatement.statements.foreach {
					statement => compile(statement)
				}
			case letStatement: LetStatement =>
				compile(letStatement.value)
				val symbol = symbolTable.define(letStatement.name.name)
				emit(OpSetGlobal, symbol.index)
			case integerLiteral: IntegerLiteral =>
				val integerObject: IntegerObject = IntegerObject(integerLiteral.value)
				emit(OpConstant, addConstant(integerObject))
			case booleanLiteral: BooleanLiteral =>
				emit(if (booleanLiteral.value) OpTrue else OpFalse)
			case identifier: Identifier =>
				val symbol = symbolTable.resolve(identifier.name)
				emit(OpGetGlobal, symbol.index)
			case _ =>
				throw new Exception(s"unknown node $node")
		}
	}

	def bytecode: Bytecode = Bytecode(instructions, constants.toList)

	private def addConstant(obj: Object): Int = {
		constants = constants :+ obj
		constants.length - 1
	}

	private def emit(operation: Opcode, operands: Int*): Int = {
		val instruction = Definition.make(operation, operands*)
		val position = addInstruction(instruction)
		setLastInstruction(operation, position)
		position
	}

	private def addInstruction(ins: Instructions): Int = {
		val positionNewInstruction = instructions.length
		instructions = instructions ++ ins
		positionNewInstruction
	}

	private def setLastInstruction(operation: Opcode, position: Int): Unit = {
		val previous = lastInstruction
		val last = EmittedInstruction(operation, position)
		previousInstruction = previous
		lastInstruction = Some(last)
	}

	private def lastInstructionIsPop: Boolean = {
		lastInstruction.getOrElse(return false).opcode == OpPop
	}

	private def removeLastPop(): Unit = {
		instructions = instructions.slice(0, lastInstruction.getOrElse(throw new Exception("no last instruction")).position)
	}

	private def replaceInstruction(position: Int, newInstruction: Instructions): Unit = {
		newInstruction.zipWithIndex.foreach {
			case (value, index) =>
				instructions(position + index) = value
		}
	}

	private def changeOperand(position: Int, operand: Int): Unit = {
		val operation = instructions(position)
		val newInstruction = Definition.make(operation, operand)
		replaceInstruction(position, newInstruction)
	}
}

case class Bytecode(instructions: Instructions, constants: List[Object])

case class EmittedInstruction(opcode: Opcode, position: Int)
