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
			List(
				Test(
					"1 + 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpAdd),
						Definition.make(OpPop),
					)
				),
				Test(
					"1 - 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpSub),
						Definition.make(OpPop),
					)
				),
				Test(
					"1 * 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpMul),
						Definition.make(OpPop),
					)
				),
				Test(
					"1 / 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpDiv),
						Definition.make(OpPop),
					)
				),
				Test(
					"1; 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpPop),
						Definition.make(OpConstant, 1),
						Definition.make(OpPop),
					)
				),
				Test(
					"-1;",
					List(1),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpMinus),
						Definition.make(OpPop),
					)
				),
			)
		)
	}

	test("compiler.booleanExpressions") {
		runCompilerTests(
			List(
				Test(
					"true;",
					List(),
					List(
						Definition.make(OpTrue),
						Definition.make(OpPop),
					)
				),
				Test(
					"false;",
					List(),
					List(
						Definition.make(OpFalse),
						Definition.make(OpPop),
					)
				),
				Test(
					"1 > 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpGreaterThan),
						Definition.make(OpPop),
					),
				),
				Test(
					"1 < 2;",
					List(2, 1),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpGreaterThan),
						Definition.make(OpPop),
					),
				),
				Test(
					"1 == 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpEqual),
						Definition.make(OpPop),
					),
				),
				Test(
					"1 != 2;",
					List(1, 2),
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpNotEqual),
						Definition.make(OpPop),
					),
				),
				Test(
					"true == false;",
					List(),
					List(
						Definition.make(OpTrue),
						Definition.make(OpFalse),
						Definition.make(OpEqual),
						Definition.make(OpPop),
					),
				),
				Test(
					"true != false;",
					List(),
					List(
						Definition.make(OpTrue),
						Definition.make(OpFalse),
						Definition.make(OpNotEqual),
						Definition.make(OpPop),
					),
				),
				Test(
					"!true;",
					List(),
					List(
						Definition.make(OpTrue),
						Definition.make(OpBang),
						Definition.make(OpPop),
					)
				)
			)
		)
	}

	test("compiler.conditionals") {
		runCompilerTests(
			List(
				Test(
					"if (true) { 10; }; 3333;",
					List(10, 3333),
					List(
						Definition.make(OpTrue),
						Definition.make(OpJumpNotTruthy, 10),
						Definition.make(OpConstant, 0),
						Definition.make(OpJump, 11),
						Definition.make(OpNull),
						Definition.make(OpPop),
						Definition.make(OpConstant, 1),
						Definition.make(OpPop),
					)
				),
				Test(
					"if (true) { 10; } else { 20; }; 3333;",
					List(10, 20, 3333),
					List(
						Definition.make(OpTrue),
						Definition.make(OpJumpNotTruthy, 10),
						Definition.make(OpConstant, 0),
						Definition.make(OpJump, 13),
						Definition.make(OpConstant, 1),
						Definition.make(OpPop),
						Definition.make(OpConstant, 2),
						Definition.make(OpPop),
					)
				),
			)
		)
	}

	test("compiler.globalLetStatements") {
		runCompilerTests(List(
			Test(
				"let one = 1; let two = 2;",
				List(1, 2),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpSetGlobal, 1),
				)
			),
			Test(
				"let one = 1; one;",
				List(1),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpGetGlobal, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"let one = 1; let two = one; two;",
				List(1),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpGetGlobal, 0),
					Definition.make(OpSetGlobal, 1),
					Definition.make(OpGetGlobal, 1),
					Definition.make(OpPop),
				)
			),
		))
	}

}
