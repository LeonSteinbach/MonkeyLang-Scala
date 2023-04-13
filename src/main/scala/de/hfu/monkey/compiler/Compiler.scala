package de.hfu.monkey.compiler

import de.hfu.monkey.ast.*
import de.hfu.monkey.objects.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*
import de.hfu.monkey.compiler.SymbolScope.*

import scala.collection.immutable.ListMap
import scala.collection.mutable

case class Compiler() {
	var constants: Array[Object] = Array[Object]()

	var symbolTable: SymbolTable = new SymbolTable()

	var scopes: List[CompilationScope] = List[CompilationScope](CompilationScope())
	var scopeIndex: Int = 0

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
				if (lastInstructionIs(OpPop))
					removeLastPop()

				val jumpPosition = emit(OpJump, 9999)
				val afterConsequencePosition = currentInstructions.length
				changeOperand(jumpNotTruthyPosition, afterConsequencePosition)

				if (ifExpression.alternative.statements.isEmpty) {
					emit(OpNull)
				} else {
					compile(ifExpression.alternative)
					if (lastInstructionIs(OpPop))
						removeLastPop()
				}
				val afterAlternativePosition = currentInstructions.length
				changeOperand(jumpPosition, afterAlternativePosition)
			case blockStatement: BlockStatement =>
				blockStatement.statements.foreach {
					statement => compile(statement)
				}
			case letStatement: LetStatement =>
				compile(letStatement.value)
				val symbol = symbolTable.define(letStatement.name.name)
				val scopeOperation = if (symbol.scope == GLOBAL) OpSetGlobal else OpSetLocal
				emit(scopeOperation, symbol.index)
			case integerLiteral: IntegerLiteral =>
				val integerObject: IntegerObject = IntegerObject(integerLiteral.value)
				emit(OpConstant, addConstant(integerObject))
			case booleanLiteral: BooleanLiteral =>
				emit(if (booleanLiteral.value) OpTrue else OpFalse)
			case identifier: Identifier =>
				val symbol = symbolTable.resolve(identifier.name)
				val scopeOperation = if (symbol.scope == GLOBAL) OpGetGlobal else OpGetLocal
				emit(scopeOperation, symbol.index)
			case stringLiteral: StringLiteral =>
				val stringObject: StringObject = StringObject(stringLiteral.value)
				emit(OpConstant, addConstant(stringObject))
			case arrayLiteral: ArrayLiteral =>
				arrayLiteral.elements.foreach {
					element => compile(element)
				}
				emit(OpArray, arrayLiteral.elements.length)
			case hashLiteral: HashLiteral =>
				val sortedKeys = hashLiteral.pairs.keys.toArray.sortBy(_.toString)
				sortedKeys.foreach {
					key =>
						compile(key)
						compile(hashLiteral.pairs(key))
				}
				emit(OpHash, hashLiteral.pairs.size * 2)
			case indexExpression: IndexExpression =>
				compile(indexExpression.left)
				compile(indexExpression.index)
				emit(OpIndex)
			case functionLiteral: FunctionLiteral =>
				enterScope()
				compile(functionLiteral.body)

				if (lastInstructionIs(OpPop))
					replaceLastPopWithReturn()
				if (!lastInstructionIs(OpReturnValue))
					emit(OpReturn)

				val instructions = leaveScope()
				val compiledFunctionObject = CompiledFunctionObject(instructions)
				emit(OpConstant, addConstant(compiledFunctionObject))
			case returnStatement: ReturnStatement =>
				compile(returnStatement.value)
				emit(OpReturnValue)
			case callExpression: CallExpression =>
				compile(callExpression.function)
				emit(OpCall)
			case _ =>
				throw new Exception(s"unknown node $node")
		}
	}

	def bytecode: Bytecode = Bytecode(currentInstructions, constants.toList)

	private def replaceLastPopWithReturn(): Unit = {
		val lastPosition = scopes(scopeIndex).lastInstruction.position
		replaceInstruction(lastPosition, Definition.make(OpReturnValue))
		scopes(scopeIndex).lastInstruction.opcode = OpReturnValue
	}

	private def currentInstructions: Instructions = scopes(scopeIndex).instructions

	private def lastInstructionIs(operation: Opcode): Boolean = {
		currentInstructions.length != 0 && scopes(scopeIndex).lastInstruction.opcode == operation
	}

	private def addConstant(obj: Object): Int = {
		constants = constants :+ obj
		constants.length - 1
	}

	def emit(operation: Opcode, operands: Int*): Int = {
		val instruction = Definition.make(operation, operands*)
		val position = addInstruction(instruction)
		setLastInstruction(operation, position)
		position
	}

	private def addInstruction(ins: Instructions): Int = {
		val positionNewInstruction = currentInstructions.length
		val updatedInstructions = currentInstructions ++ ins
		scopes(scopeIndex).instructions = updatedInstructions
		positionNewInstruction
	}

	private def setLastInstruction(operation: Opcode, position: Int): Unit = {
		val previous = scopes(scopeIndex).lastInstruction
		val last = EmittedInstruction(operation, position)
		scopes(scopeIndex).previousInstruction = previous
		scopes(scopeIndex).lastInstruction = last
	}

	private def removeLastPop(): Unit = {
		val last = scopes(scopeIndex).lastInstruction
		val previous = scopes(scopeIndex).previousInstruction

		val oldInstructions = currentInstructions
		val newInstructions = oldInstructions.slice(0, last.position)

		scopes(scopeIndex).instructions = newInstructions
		scopes(scopeIndex).lastInstruction = previous
	}

	private def replaceInstruction(position: Int, newInstruction: Instructions): Unit = {
		val ins = currentInstructions
		newInstruction.zipWithIndex.foreach {
			case (value, index) =>
				ins(position + index) = value
		}
		scopes(scopeIndex).instructions = ins
	}

	private def changeOperand(position: Int, operand: Int): Unit = {
		val operation = currentInstructions(position)
		val newInstruction = Definition.make(operation, operand)
		replaceInstruction(position, newInstruction)
	}

	def enterScope(): Unit = {
		val newScope = CompilationScope()
		scopes = scopes :+ newScope
		symbolTable = new SymbolTable(outer = Some(symbolTable))
		scopeIndex += 1
	}

	def leaveScope(): Instructions = {
		val instructions = currentInstructions
		scopes = scopes.dropRight(1)
		scopeIndex -= 1
		symbolTable = symbolTable.outer.get
		instructions
	}
}

case class Bytecode(instructions: Instructions, constants: List[Object])

case class EmittedInstruction(var opcode: Opcode = 0.toByte, position: Int = 0)

case class CompilationScope(var instructions: Instructions = Array.empty,
							var lastInstruction: EmittedInstruction = EmittedInstruction(),
							var previousInstruction: EmittedInstruction = EmittedInstruction())
