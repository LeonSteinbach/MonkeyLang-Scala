package de.hfu.monkey.compiler

import de.hfu.monkey.ast.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*

import scala.collection.mutable.ListBuffer
import scala.util.control.NonLocalReturns.{returning, throwReturn}

case class Compiler() {
	var instructions: Instructions = Array[Byte]()
	var constants: Array[Object] = Array[Object]()

	def compile(node: Node): Option[Exception] = returning {
		node match {
			case program: Program =>
				program.statements.foreach { statement =>
					compile(statement) match {
						case Some(exception: Exception) => throwReturn(Some(exception))
						case _ =>
					}
				}
			case expressionStatement: ExpressionStatement =>
				compile(expressionStatement.expression) match {
					case Some(exception: Exception) => throwReturn(Some(exception))
					case _ =>
				}
			case infixExpression: InfixExpression =>
				compile(infixExpression.left) match {
					case Some(exception: Exception) => throwReturn(Some(exception))
					case _ =>
				}
				compile(infixExpression.right) match {
					case Some(exception: Exception) => throwReturn(Some(exception))
					case _ =>
				}
			case integerLiteral: IntegerLiteral =>
				val integerObject: IntegerObject = IntegerObject(integerLiteral.value)
				emit(OpConstant, Array(addConstant(integerObject)))
			case _ =>
				throwReturn(None)
		}
		None
	}

	def bytecode: Bytecode = Bytecode(instructions, constants.toList)

	private def addConstant(obj: Object): Int = {
		constants = constants :+ obj
		constants.length - 1
	}

	private def emit(operation: Opcode, operands: Array[Int]): Int = {
		val instruction = Definition.make(operation, operands*)
		val position = addInstruction(instruction)
		position
	}

	private def addInstruction(ins: Array[Byte]): Int = {
		val positionNewInstruction = instructions.length
		instructions = instructions ++ ins
		positionNewInstruction
	}
}

case class Bytecode(instructions: Instructions, constants: List[Object])
