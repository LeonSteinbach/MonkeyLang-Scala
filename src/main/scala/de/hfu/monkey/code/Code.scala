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
			val (definition, err) = Definition.lookup(instruction(i))
			if (err.isDefined) {
				out.append(s"ERROR: ${err.get}\n")
			} else {
				val (operands, read) = readOperands(definition.get, instruction.slice(i + 1, i + 1 + definition.get.operandWidths.sum))
				out.append(f"$i%04d ${instruction.fmtInstruction(definition.get, operands)}\n")
				i += 1 + read
			}
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
}

case class Definition(name: String, operandWidths: Array[Int])

object Definition {
	private val definitions: Map[Opcode, Definition] = Map[Opcode, Definition](
		OpConstant -> Definition("OpConstant", Array(2)),
		OpAdd -> Definition("OpAdd", Array())
	)

	def lookup(operation: Byte): (Option[Definition], Option[Exception]) = {
		definitions.get(operation) match {
			case Some(definition: Definition) => (Some(definition), None)
			case None => (None, Some(new Exception(s"opcode $operation undefined")))
		}
	}

	def make(operation: Opcode, operands: Int*): Array[Byte] = {
		definitions.get(operation) match {
			case Some(definition) =>
				val instruction = new Array[Byte](definition.operandWidths.sum + 1)
				var offset = 1
				operands.zip(definition.operandWidths).foreach {
					case (value, width) =>
						ByteBuffer.wrap(instruction, offset, width).order(ByteOrder.BIG_ENDIAN).putShort(value.toShort)
						offset += width
				}
				instruction(0) = operation
				instruction
			case None =>
				Array.emptyByteArray
		}
	}

}
