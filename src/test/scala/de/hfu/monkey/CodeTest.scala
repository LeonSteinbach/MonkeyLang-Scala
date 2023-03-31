package de.hfu.monkey

import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.{OpConstant, Opcode}
import de.hfu.monkey.code.{Definition, Instructions, Opcode}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.{ByteBuffer, ByteOrder}

class CodeTest extends AnyFunSuite {

	case class Test(op: Opcode, operands: Array[Int], bytesRead: Int)

	test("code.make") {
		val instruction: Array[Byte] = Definition.make(Opcode.OpConstant, Array(65534))
		val expected: Array[Byte] = Array(Opcode.OpConstant, 255.toByte, 254.toByte)
		assert(instruction === expected)
	}

	test("code.instructionsString") {
		val instructions: List[Instructions] = List(
			Definition.make(Opcode.OpConstant, Array(1)),
			Definition.make(Opcode.OpConstant, Array(2)),
			Definition.make(Opcode.OpConstant, Array(65535)),
		)
		val expected: String =
			"0000 OpConstant 1\n" +
			"0003 OpConstant 2\n" +
			"0006 OpConstant 65535\n"

		val contacted: Array[Opcode] = instructions.flatten.toArray
		if (contacted.inspect != expected)
			fail(s"instructions wrongly formatted.\nwant $expected\ngot ${contacted.inspect}")
	}

	extension (ins: Instructions) {
		def inspect: String = {
			val out = new StringBuilder()
			var i = 0
			while (i < ins.length) {
				val (definition, err) = Definition.lookup(ins(i))
				if (err.isDefined) {
					out.append(s"ERROR: ${err.get}\n")
				} else {
					val (operands, read) = readOperands(definition.get, ins.slice(i + 1, i + 1 + definition.get.operandWidths.sum))
					out.append(f"$i%04d ${ins.fmtInstruction(definition.get, operands)}\n")
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

	test("code.readOperands") {
		val tests = List(
			Test(Opcode.OpConstant, Array(65535), 2)
		)

		for (test <- tests) {
			val instruction: Array[Byte] = Definition.make(test.op, test.operands)

			val (definition, err) = Definition.lookup(test.op)
			if (err.isDefined) {
				fail(s"definition not found: ${err.get}")
			}

			val (operandsRead, n) = readOperands(definition.get, instruction.tail)
			if (n != test.bytesRead) {
				fail(s"n wrong. want ${test.bytesRead} got $n")
			}

			for ((want, got) <- test.operands.zip(operandsRead)) {
				if (got != want) {
					fail(s"operand wrong. want $want got $got")
				}
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
}