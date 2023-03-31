package de.hfu.monkey

import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*
import org.scalatest.funsuite.AnyFunSuite

import java.nio.{ByteBuffer, ByteOrder}

class CodeTest extends AnyFunSuite {

	case class Test(op: Opcode, operands: Array[Int], bytesRead: Int)

	test("code.make") {
		val instructions: List[Instructions] = List(
			Definition.make(Opcode.OpConstant, 1),
			Definition.make(Opcode.OpConstant, 2),
			Definition.make(Opcode.OpConstant, 65535),
			Definition.make(Opcode.OpAdd),
		)

		val expected: List[Instructions] = List(
			Array(Opcode.OpConstant, 0.toByte, 1.toByte),
			Array(Opcode.OpConstant, 0.toByte, 2.toByte),
			Array(Opcode.OpConstant, 255.toByte, 255.toByte),
			Array(Opcode.OpAdd)
		)

		instructions.zip(expected).foreach { case (instruction, expInstruction) =>
			assert(instruction === expInstruction)
		}
	}

	test("code.instructionsString") {
		val instructions: List[Instructions] = List(
			Definition.make(Opcode.OpAdd),
			Definition.make(Opcode.OpConstant, 2),
			Definition.make(Opcode.OpConstant, 65535),
		)
		val expected: String =
			"0000 OpAdd\n" +
			"0001 OpConstant 2\n" +
			"0004 OpConstant 65535\n"

		val contacted: Array[Opcode] = instructions.flatten.toArray
		if (contacted.inspect != expected)
			fail(s"instructions wrongly formatted.\nwant $expected\ngot ${contacted.inspect}")
	}

	test("code.readOperands") {
		val tests = List(
			Test(Opcode.OpConstant, Array(65535), 2)
		)

		for (test <- tests) {
			val instruction: Array[Byte] = Definition.make(test.op, test.operands*)

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

}