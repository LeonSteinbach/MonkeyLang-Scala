package de.hfu.monkey.code

import de.hfu.monkey.code.Opcode.*

import java.nio.{ByteBuffer, ByteOrder}

case class UnsignedByte(byte: Short) extends AnyVal {
	override def toString: String = s"${byte}u"

	def &(i: Int): Int = byte & i

	def toInt: Int = byte
}

extension (int: Int) {
	def toUnsignedByte: UnsignedByte = UnsignedByte(int.toShort)
}

type Instructions = Array[UnsignedByte]

extension (instruction: Instructions) {
	def offset(offset: Int): Instructions = instruction.slice(offset, instruction.length)

	def writeChar(offset: Int, value: Int): Unit = {
		instruction(offset) = ((value >>> 8) & 255).toUnsignedByte
		instruction(offset + 1) = ((value >>> 0) & 255).toUnsignedByte
	}

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

	def readByte: UnsignedByte = instruction.read(0).toUnsignedByte

	def readByte(offset: Int): UnsignedByte = instruction.offset(offset).readByte

	def read(position: Int): Int = instruction(position) & 255

	def inspect: String = {
		val out = new StringBuilder()
		var i = 0
		while (i < instruction.length) {
			val definition = Definition.lookup(instruction(i))
			val (operands, read) = readOperands(definition, instruction.offset(i + 1))
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
			case 2 => s"${definition.name} ${operands(0)} ${operands(1)}"
			case _ => s"ERROR: unhandled operandCount for ${definition.name}\n"
		}
	}
}

def readOperands(definition: Definition, instructions: Instructions): (Array[Int], Int) = {
	val operands = Array.fill[Int](definition.operandWidths.length)(0)
	var offset = 0

	for ((width, i) <- definition.operandWidths.zipWithIndex) {
		width match {
			case 2 => operands(i) = instructions.readInt(offset)
			case 1 => operands(i) = instructions.offset(offset).readByte.toInt
			case _ => operands(i) = width
		}
		offset += width
	}

	(operands, offset)
}

object Opcode extends Enumeration {
	type Opcode = UnsignedByte
	val OpConstant: Opcode = 0.toUnsignedByte
	val OpAdd: Opcode = 1.toUnsignedByte
	val OpSub: Opcode = 2.toUnsignedByte
	val OpMul: Opcode = 3.toUnsignedByte
	val OpDiv: Opcode = 4.toUnsignedByte
	val OpPop: Opcode = 5.toUnsignedByte
	val OpTrue: Opcode = 6.toUnsignedByte
	val OpFalse: Opcode = 7.toUnsignedByte
	val OpEqual: Opcode = 8.toUnsignedByte
	val OpNotEqual: Opcode = 9.toUnsignedByte
	val OpGreaterThan: Opcode = 10.toUnsignedByte
	val OpMinus: Opcode = 11.toUnsignedByte
	val OpBang: Opcode = 12.toUnsignedByte
	val OpJumpNotTruthy: Opcode = 13.toUnsignedByte
	val OpJump: Opcode = 14.toUnsignedByte
	val OpNull: Opcode = 15.toUnsignedByte
	val OpGetGlobal: Opcode = 16.toUnsignedByte
	val OpSetGlobal: Opcode = 17.toUnsignedByte
	val OpArray: Opcode = 18.toUnsignedByte
	val OpHash: Opcode = 19.toUnsignedByte
	val OpIndex: Opcode = 20.toUnsignedByte
	val OpCall: Opcode = 21.toUnsignedByte
	val OpReturnValue: Opcode = 22.toUnsignedByte
	val OpReturn: Opcode = 23.toUnsignedByte
	val OpGetLocal: Opcode = 24.toUnsignedByte
	val OpSetLocal: Opcode = 25.toUnsignedByte
	val OpClosure: Opcode = 26.toUnsignedByte
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
		OpCall -> Definition("OpCall", Array(1)),
		OpReturnValue -> Definition("OpReturnValue", Array()),
		OpReturn -> Definition("OpReturn", Array()),
		OpGetLocal -> Definition("OpGetLocal", Array(1)),
		OpSetLocal -> Definition("OpSetLocal", Array(1)),
		OpClosure -> Definition("OpClosure", Array(2, 1)),
	)

	def lookup(operation: Opcode): Definition = {
		definitions.getOrElse(operation, throw new Exception(s"opcode $operation undefined"))
	}

	def make(operation: Opcode, operands: Int*): Instructions = {
		val definition = lookup(operation)

		val instruction = new Instructions(definition.operandWidths.sum + 1)
		instruction(0) = operation
		var offset = 1
		operands.zipWithIndex.foreach { case (value, i) =>
			val width = definition.operandWidths(i)
			width match {
				case 2 => instruction.writeChar(offset, value)
				case 1 => instruction(offset) = value.toUnsignedByte
			}
			offset += width
		}

		instruction
	}

}
