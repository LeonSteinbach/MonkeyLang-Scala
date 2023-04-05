package de.hfu.monkey.code

import de.hfu.monkey.code.Opcode.*
import java.nio.{ByteBuffer, ByteOrder}

type Instructions = Array[Byte]

extension (instruction: Instructions) {
	def offset(offset: Int): Instructions = instruction.slice(offset, instruction.length)

	def readInt(offset: Int): Int = instruction.offset(offset).readChar

	def readChar: Char = {
		val ch1 = instruction.read(0)
		val ch2 = instruction.read(1)
		if ((ch1 | ch2) < 0) {
			throw IllegalStateException()
		} else {
			((ch1 << 8) + (ch2 << 0)).toChar
		}
	}

	def read(position: Int): Int = instruction(position) & 255

	def inspect: String = {
		val out = new StringBuilder()
		var i = 0
		while (i < instruction.length) {
			val definition = Definition.lookup(instruction(i))
			val (operands, read) = readOperands(definition, instruction.slice(i + 1, i + 1 + definition.operandWidths.sum))
			out.append(f"$i%04d ${instruction.fmtInstruction(definition, operands)}\n")
			i += 1 + read
		}
		out.toString()
	}

	def fmtInstruction(definition: Definition, operands: Array[Int]): String = {
		val operandCount = definition.operandWidths.length

		if (operands.length != operandCount) {
			return s"ERROR: operand len ${operands.length} does not match defined $operandCount\n"
		}

		operandCount match {
			case 0 => definition.name
			case 1 => s"${definition.name} ${operands(0)}"
			case _ => s"ERROR: unhandled operandCount for ${definition.name}\n"
		}
	}
}

def readOperands(definition: Definition, instructions: Instructions): (Array[Int], Int) = {
	val operands = Array.fill[Int](definition.operandWidths.length)(0)
	var offset = 0

	for ((width, i) <- definition.operandWidths.zipWithIndex) {
		width match {
			case 2 =>
				operands(i) = instructions.readInt(offset)
		}
		offset += width
	}

	(operands, offset)
}

object Opcode extends Enumeration {
	type Opcode = Byte
	val OpConstant: Opcode = 0
	val OpAdd: Opcode = 1
	val OpSub: Opcode = 2
	val OpMul: Opcode = 3
	val OpDiv: Opcode = 4
	val OpPop: Opcode = 5
	val OpTrue: Opcode = 6
	val OpFalse: Opcode = 7
	val OpEqual: Opcode = 8
	val OpNotEqual: Opcode = 9
	val OpGreaterThan: Opcode = 10
	val OpMinus: Opcode = 11
	val OpBang: Opcode = 12
	val OpJumpNotTruthy: Opcode = 13
	val OpJump: Opcode = 14
	val OpNull: Opcode = 15
	val OpGetGlobal: Opcode = 16
	val OpSetGlobal: Opcode = 17
	val OpArray: Opcode = 18
	val OpHash: Opcode = 19
	val OpIndex: Opcode = 20
}

case class Definition(name: String, operandWidths: Array[Int])

object Definition {
	private val definitions: Map[Opcode, Definition] = Map[Opcode, Definition](
		OpConstant -> Definition("OpConstant", Array(2)),
		OpAdd -> Definition("OpAdd", Array()),
		OpSub -> Definition("OpSub", Array()),
		OpMul -> Definition("OpMul", Array()),
		OpDiv -> Definition("OpDiv", Array()),
		OpPop -> Definition("OpPop", Array()),
		OpTrue -> Definition("OpTrue", Array()),
		OpFalse -> Definition("OpFalse", Array()),
		OpEqual -> Definition("OpEqual", Array()),
		OpNotEqual -> Definition("OpNotEqual", Array()),
		OpGreaterThan -> Definition("OpGreaterThan", Array()),
		OpMinus -> Definition("OpMinus", Array()),
		OpBang -> Definition("OpBang", Array()),
		OpJumpNotTruthy -> Definition("OpJumpNotTruthy", Array(2)),
		OpJump -> Definition("OpJump", Array(2)),
		OpNull -> Definition("OpNull", Array()),
		OpGetGlobal -> Definition("OpGetGlobal", Array(2)),
		OpSetGlobal -> Definition("OpSetGlobal", Array(2)),
		OpArray -> Definition("OpArray", Array(2)),
		OpHash -> Definition("OpHash", Array(2)),
		OpIndex -> Definition("OpIndex", Array()),
	)

	def lookup(operation: Opcode): Definition = {
		definitions.getOrElse(operation, throw new Exception(s"opcode $operation undefined"))
	}

	def make(operation: Opcode, operands: Int*): Instructions = {
		val definition = lookup(operation)

		val instruction = new Instructions(definition.operandWidths.sum + 1)
		var offset = 1
		operands.zip(definition.operandWidths).foreach {
			case (value, width) =>
				ByteBuffer.wrap(instruction, offset, width).order(ByteOrder.BIG_ENDIAN).putShort(value.toShort)
				offset += width
		}
		instruction(0) = operation
		instruction
	}

}
