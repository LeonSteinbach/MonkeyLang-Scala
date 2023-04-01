package de.hfu.monkey

import de.hfu.monkey.ast.*
import de.hfu.monkey.objects.*
import de.hfu.monkey.code.*
import de.hfu.monkey.code.Opcode.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.lexer.Lexer
import de.hfu.monkey.parser.ManualParser
import org.scalatest.funsuite.AnyFunSuite

import scala.util.control.NonLocalReturns.{returning, throwReturn}

class CompilerTest extends AnyFunSuite {

	case class Test(input: String, expectedConstants: List[Int], expectedInstructions: List[Instructions])

	private def runCompilerTests(tests: List[Test]): Unit = {
		tests.foreach { test =>
			val program = ManualParser().parse(test.input)

			val compiler = Compiler()
			compiler.compile(program)

			val bytecode = compiler.bytecode

			testInstructions(test.expectedInstructions, bytecode.instructions)
			testConstants(test.expectedConstants, bytecode.constants)
		}
	}

	private def testInstructions(expected: List[Instructions], actual: Instructions): Unit = {
		val contacted = concatInstructions(expected)
		if (actual.length != contacted.length) {
			fail(s"wrong instructions length.\n\nwant\n${contacted.inspect}\ngot\n${actual.inspect}")
		} else {
			actual.zip(contacted).zipWithIndex.foreach {
				case ((value, expectedValue), index) =>
					if (value != expectedValue) {
						fail(s"wrong instruction at $index. want $expectedValue got $value")
					}
			}
		}
	}

	private def testConstants(expected: List[Int], actual: List[objects.Object]): Unit = {
		if (expected.length != actual.length) {
			fail(s"wrong number of constants. got ${actual.length} want ${expected.length}")
		} else {
			expected.zipWithIndex.foreach {
				case (value, index) =>
					value match {
						case integer: Int => testIntegerObject(integer, actual(index))
					}
			}
		}
	}

	private def testIntegerObject(expected: Int, actual: objects.Object): Unit = {
		actual match {
			case integer: IntegerObject =>
				if (integer.value != expected)
					fail(s"object has wrong value. got ${integer.value} want $expected")
			case _ => fail(s"object is not an Integer. got ${actual.`type`()}")
		}
	}

	private def concatInstructions(instructions: List[Instructions]): Instructions = {
		var out: Array[Byte] = Array.emptyByteArray
		for (ins <- instructions) {
			out = Array.concat(out, ins)
		}
		out
	}

	test("compiler.integerArithmetic") {
		runCompilerTests(
			List(Test(
				"1 + 2;",
				List(1, 2),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpAdd),
				)
			))
		)
	}

}
