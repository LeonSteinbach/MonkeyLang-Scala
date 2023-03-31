package de.hfu.monkey.code

import de.hfu.monkey.code.Opcode.{OpConstant, Opcode}

import java.nio.{ByteBuffer, ByteOrder}

type Instructions = Array[Byte]

extension (i: Instructions) {
	def offset(offset: Int): Instructions = i.slice(offset, i.length)
	def readInt(offset: Int): Int = i.offset(offset).readChar
	def readChar: Char = {
		val ch1 = i.read(0)
		val ch2 = i.read(1)
		if ((ch1 | ch2) < 0) {
			throw IllegalStateException()
		} else {
			((ch1 << 8) + (ch2 << 0)).toChar
		}
	}
	def read(position: Int): Int = i(position) & 255
}

object Opcode extends Enumeration {
	type Opcode = Byte
	val OpConstant: Opcode = 0
}

case class Definition(name: String, operandWidths: Array[Int])

object Definition {
	private val definitions: Map[Opcode, Definition] = Map[Opcode, Definition](
		OpConstant -> Definition("OpConstant", Array(2))
	)

	def lookup(operation: Byte): (Option[Definition], Option[Exception]) = {
		definitions.get(operation) match {
			case Some(definition: Definition) => (Some(definition), None)
			case None => (None, Some(new Exception(s"opcode $operation undefined")))
		}
	}

	def make(operation: Opcode, operands: Array[Int]): Array[Byte] = {
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
