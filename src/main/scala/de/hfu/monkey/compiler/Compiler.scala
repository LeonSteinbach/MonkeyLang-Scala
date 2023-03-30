package de.hfu.monkey.compiler

import de.hfu.monkey.ast.Node
import de.hfu.monkey.evaluator.Object
import de.hfu.monkey.code.Instructions

case class Compiler() {
	val instructions: Instructions = Array[Byte]()
	val constants: List[Object] = List[Object]()

	def compile(node: Node): Option[Exception] = {
		None
	}

	def bytecode: Bytecode = Bytecode(instructions, constants)
}

case class Bytecode(instructions: Instructions, constants: List[Object])
