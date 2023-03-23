package de.hfu.monkey

import de.hfu.monkey.code.Opcode.Opcode
import de.hfu.monkey.code.{Definition, Opcode}
import org.scalatest.funsuite.AnyFunSuite

class CodeTest extends AnyFunSuite {

	test("code.make") {
		val instruction: Array[Byte] = Definition.make(Opcode.OpConstant, Array(65534))
		val expected: Array[Byte] = Array(Opcode.OpConstant, 255.toByte, 254.toByte)
		assert(instruction === expected)
	}
}