package de.hfu.monkey.compiler

import de.hfu.monkey.ast.*
import de.hfu.monkey.objects.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*

case class Compiler() {
	var instructions: Instructions = Array[Byte]()
	var constants: Array[Object] = Array[Object]()

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
			case integerLiteral: IntegerLiteral =>
				val integerObject: IntegerObject = IntegerObject(integerLiteral.value)
				emit(OpConstant, addConstant(integerObject))
			case booleanLiteral: BooleanLiteral =>
				emit(if (booleanLiteral.value) OpTrue else OpFalse)
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
		position
	}

	private def addInstruction(ins: Instructions): Int = {
		val positionNewInstruction = instructions.length
		instructions = instructions ++ ins
		positionNewInstruction
	}
}

case class Bytecode(instructions: Instructions, constants: List[Object])
