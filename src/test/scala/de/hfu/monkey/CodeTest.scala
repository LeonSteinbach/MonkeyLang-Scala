package de.hfu.monkey

import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*
import org.scalatest.funsuite.AnyFunSuite

import java.nio.{ByteBuffer, ByteOrder}

class CodeTest extends AnyFunSuite {

	case class Test(operation: Opcode, operands: Array[Int], bytesRead: Int)

	test("code.make") {
		val instructions: List[Instructions] = List(
			Definition.make(OpConstant, 1),
			Definition.make(OpConstant, 2),
			Definition.make(OpConstant, 65535),
			Definition.make(OpAdd),
		)

		val expected: List[Instructions] = List(
			Array(OpConstant, 0.toByte, 1.toByte),
			Array(OpConstant, 0.toByte, 2.toByte),
			Array(OpConstant, 255.toByte, 255.toByte),
			Array(OpAdd)
		)

		instructions.zip(expected).foreach { case (instruction, expInstruction) =>
			assert(instruction === expInstruction)
		}
	}

	test("code.instructionsString") {
		val instructions: List[Instructions] = List(
			Definition.make(OpAdd),
			Definition.make(OpConstant, 2),
			Definition.make(OpConstant, 65535),
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
			Test(OpConstant, Array(65535), 2)
		)

		for (test <- tests) {
			val instruction: Array[Byte] = Definition.make(test.operation, test.operands*)

			val definition = Definition.lookup(test.operation)

			val (operandsRead, n) = readOperands(definition, instruction.tail)
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