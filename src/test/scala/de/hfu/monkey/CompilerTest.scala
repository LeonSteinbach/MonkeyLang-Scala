package de.hfu.monkey

import de.hfu.monkey.ast.Program
import de.hfu.monkey.code.Opcode.OpConstant
import de.hfu.monkey.code.{Definition, Instructions}
import de.hfu.monkey.compiler.{Bytecode, Compiler}
import de.hfu.monkey.evaluator.{IntegerObject, Object}
import de.hfu.monkey.lexer.Lexer
import de.hfu.monkey.parser.ManualParser
import org.scalatest.funsuite.AnyFunSuite

import scala.util.control.NonLocalReturns.{returning, throwReturn}

class CompilerTest extends AnyFunSuite {

	import de.hfu.monkey.code.Opcode

	private def runCompilerTest(input: String, expectedConstants: Array[Int], expectedInstructions: Instructions): Boolean = {
		val parser = ManualParser()
		val node = parser.parse(input)
		val compiler = Compiler()

		val compilerError = compiler.compile(node)
		assert(compilerError.isEmpty, s"compiler error: ${compilerError.getOrElse(None)}")

		val bytecode = compiler.bytecode

		val instructionsError = testInstructions(expectedInstructions, bytecode.instructions)
		assert(instructionsError.isEmpty, s"testInstructions failed: ${instructionsError.get}")

		val constantsError = testConstants(expectedConstants, bytecode.constants)
		assert(constantsError.isEmpty, s"testConstants failed: ${constantsError.get}")

		true
	}

	private def testInstructions(expected: Instructions, actual: Instructions): Option[String] = returning {
		val contacted = concatInstructions(expected)
		if (actual.length != contacted.length) {
			Some(s"wrong instructions length. want ${contacted.length} got ${actual.length}")
		} else {
			actual.zip(contacted).zipWithIndex.foreach {
				case ((value, expectedValue), index) =>
					if (value != expectedValue) {
						throwReturn(Some(s"wrong instruction at $index. want $expectedValue got $value"))
					}
			}
			None
		}
	}

	private def testConstants(expected: Array[Int], actual: List[Object]): Option[String] = returning {
		if (expected.length != actual.length) {
			Some(s"wrong number of constants. got ${actual.length} want ${expected.length}")
		} else {
			expected.zipWithIndex.foreach {
				case (value, index) =>
					value match {
						case integer: Int =>
							testIntegerObject(integer, actual(index)) match {
								case Some(errorMessage) => throwReturn(Some(s"constant $index - $errorMessage"))
								case None =>
							}
					}
			}
			None
		}
	}

	private def testIntegerObject(expected: Int, actual: Object): Option[String] = {
		actual match {
			case integer: IntegerObject =>
				if (integer.value != expected) {
					Some(s"object has wrong value. got ${integer.value} want $expected")
				} else {
					None
				}
			case _ => Some(s"object is not an Integer. got ${actual.`type`()}")
		}
	}

	private def concatInstructions(instructions: Instructions): Instructions = {
		instructions.foldLeft(Array[Byte]())((acc, i) => acc ++ Array(i))
	}

	test("compiler.integerArithmetic") {
		runCompilerTest(
			"1 + 2",
			Array[Int](1, 2),
			Array(OpConstant, 0, OpConstant, 1)
		)
	}

}
