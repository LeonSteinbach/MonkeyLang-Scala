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

	case class Test(input: String, expectedConstants: List[Any], expectedInstructions: List[Instructions])

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

	private def testConstants(expected: List[Any], actual: List[Object]): Unit = {
		if (expected.length != actual.length) {
			fail(s"wrong number of constants. got ${actual.length} want ${expected.length}")
		} else {
			expected.zipWithIndex.foreach {
				case (constant, index) =>
					constant match {
						case integer: Int => testIntegerObject(integer, actual(index))
						case string: String => testStringObject(string, actual(index))
						case list: List[?] =>
							actual(index) match {
								case compiledFunctionObject: CompiledFunctionObject =>
									testInstructions(list.asInstanceOf[List[Instructions]], compiledFunctionObject.instructions)
								case other => fail(s"constant $other not a function")
							}
					}
			}
		}
	}

	private def testIntegerObject(expected: Int, actual: Object): Unit = {
		actual match {
			case integer: IntegerObject =>
				if (integer.value != expected)
					fail(s"object has wrong value. got ${integer.value} want $expected")
			case _ => fail(s"object is not an Integer. got ${actual.`type`()}")
		}
	}

	private def testStringObject(expected: String, actual: Object): Unit = {
		actual match {
			case string: StringObject =>
				if (string.value != expected)
					fail(s"object has wrong value. got ${string.value} want $expected")
			case _ => fail(s"object is not a String. got ${actual.`type`()}")
		}
	}

	private def concatInstructions(instructions: List[Instructions]): Instructions = {
		var out: Instructions = Array.empty[UnsignedByte]
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

	test("compiler.stringExpressions") {
		runCompilerTests(List(
			Test(
				"\"monkey\";",
				List("monkey"),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"\"mon\" + \"key\";",
				List("mon", "key"),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpAdd),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.arrayLiterals") {
		runCompilerTests(List(
			Test(
				"[];",
				List(),
				List(
					Definition.make(OpArray, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"[1, 2, 3];",
				List(1, 2, 3),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpConstant, 2),
					Definition.make(OpArray, 3),
					Definition.make(OpPop),
				)
			),
			Test(
				"[1 + 2, 3 - 4, 5 * 6];",
				List(1, 2, 3, 4, 5, 6),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpAdd),
					Definition.make(OpConstant, 2),
					Definition.make(OpConstant, 3),
					Definition.make(OpSub),
					Definition.make(OpConstant, 4),
					Definition.make(OpConstant, 5),
					Definition.make(OpMul),
					Definition.make(OpArray, 3),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.hashLiterals") {
		runCompilerTests(List(
			Test(
				"{};",
				List(),
				List(
					Definition.make(OpHash, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"{1: 2, 3: 4, 5: 6};",
				List(1, 2, 3, 4, 5, 6),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpConstant, 2),
					Definition.make(OpConstant, 3),
					Definition.make(OpConstant, 4),
					Definition.make(OpConstant, 5),
					Definition.make(OpHash, 6),
					Definition.make(OpPop),
				)
			),
			Test(
				"{1: true, \"a\": 2 + 3, false: 4};",
				List(4, 1, "a", 2, 3),
				List(
					Definition.make(OpFalse),
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpTrue),
					Definition.make(OpConstant, 2),
					Definition.make(OpConstant, 3),
					Definition.make(OpConstant, 4),
					Definition.make(OpAdd),
					Definition.make(OpHash, 6),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.indexExpressions") {
		runCompilerTests(List(
			Test(
				"[1, 2, 3][1 + 1];",
				List(1, 2, 3, 1, 1),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpConstant, 2),
					Definition.make(OpArray, 3),
					Definition.make(OpConstant, 3),
					Definition.make(OpConstant, 4),
					Definition.make(OpAdd),
					Definition.make(OpIndex),
					Definition.make(OpPop),
				)
			),
			Test(
				"{1: 2, 3: 4}[2 - 1];",
				List(1, 2, 3, 4, 2, 1),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpConstant, 2),
					Definition.make(OpConstant, 3),
					Definition.make(OpHash, 4),
					Definition.make(OpConstant, 4),
					Definition.make(OpConstant, 5),
					Definition.make(OpSub),
					Definition.make(OpIndex),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.functions") {
		runCompilerTests(List(
			Test(
				"fn() { return 5 + 10; };",
				List(
					5,
					10,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpAdd),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 2, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"fn() { 5 + 10; };",
				List(
					5,
					10,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpAdd),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 2, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"fn() { 1; 2; };",
				List(
					1,
					2,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpPop),
						Definition.make(OpConstant, 1),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 2, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"fn() { };",
				List(
					List(
						Definition.make(OpReturn),
					)
				),
				List(
					Definition.make(OpClosure, 0, 0),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.functionCalls") {
		runCompilerTests(List(
			Test(
				"fn() { 123; }();",
				List(
					123,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 1, 0),
					Definition.make(OpCall, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"let foo = fn() { 123; }; foo();",
				List(
					123,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 1, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpGetGlobal, 0),
					Definition.make(OpCall, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"let foo = fn(a) { a; }; foo(123);",
				List(
					List(
						Definition.make(OpGetLocal, 0),
						Definition.make(OpReturnValue),
					),
					123
				),
				List(
					Definition.make(OpClosure, 0, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpGetGlobal, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpCall, 1),
					Definition.make(OpPop),
				)
			),
			Test(
				"let foo = fn(a, b, c) { a; b; c; }; foo(1, 2, 3);",
				List(
					List(
						Definition.make(OpGetLocal, 0),
						Definition.make(OpPop),
						Definition.make(OpGetLocal, 1),
						Definition.make(OpPop),
						Definition.make(OpGetLocal, 2),
						Definition.make(OpReturnValue),
					),
					1,
					2,
					3
				),
				List(
					Definition.make(OpClosure, 0, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpGetGlobal, 0),
					Definition.make(OpConstant, 1),
					Definition.make(OpConstant, 2),
					Definition.make(OpConstant, 3),
					Definition.make(OpCall, 3),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.scopes") {
		val compiler = Compiler()
		if (compiler.scopeIndex != 0)
			fail(s"scopeIndex wrong. got ${compiler.scopeIndex}, want 0")

		val globalSymbolTable = compiler.symbolTable

		compiler.emit(OpMul)

		compiler.enterScope()

		if (compiler.scopeIndex != 1)
			fail(s"scopeIndex wrong. got ${compiler.scopeIndex}, want 1")

		compiler.emit(OpSub)

		if (compiler.scopes(compiler.scopeIndex).instructions.length != 1)
			fail(s"instructions length wrong. got ${compiler.scopes(compiler.scopeIndex).instructions.length}, want 1")

		var last = compiler.scopes(compiler.scopeIndex).lastInstruction
		if (last.opcode != OpSub)
			fail(s"lastInstruction opcode wrong. got ${last.opcode}, want $OpSub")

		if (compiler.symbolTable.outer.get != globalSymbolTable)
			fail("compiler did not enclose symbol table")

		compiler.leaveScope()

		if (compiler.scopeIndex != 0)
			fail(s"scopeIndex wrong. got ${compiler.scopeIndex}, want 0")

		if (compiler.symbolTable != globalSymbolTable)
			fail("compiler did not restore global symbol table")

		if (compiler.symbolTable.outer.isDefined)
			fail("compiler modified global symbol table incorrectly")

		compiler.emit(OpAdd)

		if (compiler.scopes(compiler.scopeIndex).instructions.length != 2)
			fail(s"instructions length wrong. got ${compiler.scopes(compiler.scopeIndex).instructions.length}, want 2")

		last = compiler.scopes(compiler.scopeIndex).lastInstruction
		if (last.opcode != OpAdd)
			fail(s"lastInstruction opcode wrong. got ${last.opcode}, want $OpAdd")

		val previous = compiler.scopes(compiler.scopeIndex).previousInstruction
		if (previous.opcode != OpMul)
			fail(s"previousInstruction opcode wrong. got ${previous.opcode}, want $OpMul")
	}

	test("compiler.letStatementScopes") {
		runCompilerTests(List(
			Test(
				"let num = 55; fn() { num; };",
				List(
					55,
					List(
						Definition.make(OpGetGlobal, 0),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpClosure, 1, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"fn() { let num = 55; num; };",
				List(
					55,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpSetLocal, 0),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 1, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"fn() { let a = 55; let b = 77; a + b; };",
				List(
					55,
					77,
					List(
						Definition.make(OpConstant, 0),
						Definition.make(OpSetLocal, 0),
						Definition.make(OpConstant, 1),
						Definition.make(OpSetLocal, 1),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpGetLocal, 1),
						Definition.make(OpAdd),
						Definition.make(OpReturnValue),
					)
				),
				List(
					Definition.make(OpClosure, 2, 0),
					Definition.make(OpPop),
				)
			),
		))
	}

	test("compiler.closures") {
		runCompilerTests(List(
			Test(
				"fn(a) { fn(b) { a + b; }; };",
				List(
					List(
						Definition.make(OpGetFree, 0),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpAdd),
						Definition.make(OpReturnValue),
					),
					List(
						Definition.make(OpGetLocal, 0),
						Definition.make(OpClosure, 0, 1),
						Definition.make(OpReturnValue),
					),
				),
				List(
					Definition.make(OpClosure, 1, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"fn(a) { fn(b) { fn(c) { a + b + c; }; }; };",
				List(
					List(
						Definition.make(OpGetFree, 0),
						Definition.make(OpGetFree, 1),
						Definition.make(OpAdd),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpAdd),
						Definition.make(OpReturnValue),
					),
					List(
						Definition.make(OpGetFree, 0),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpClosure, 0, 2),
						Definition.make(OpReturnValue),
					),
					List(
						Definition.make(OpGetLocal, 0),
						Definition.make(OpClosure, 1, 1),
						Definition.make(OpReturnValue),
					),
				),
				List(
					Definition.make(OpClosure, 2, 0),
					Definition.make(OpPop),
				)
			),
			Test(
				"let global = 55; fn() { let a = 66; fn() { let b = 77; fn() { let c = 88; global + a + b + c; }; }; };",
				List(
					55,
					66,
					77,
					88,
					List(
						Definition.make(OpConstant, 3),
						Definition.make(OpSetLocal, 0),
						Definition.make(OpGetGlobal, 0),
						Definition.make(OpGetFree, 0),
						Definition.make(OpAdd),
						Definition.make(OpGetFree, 1),
						Definition.make(OpAdd),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpAdd),
						Definition.make(OpReturnValue),
					),
					List(
						Definition.make(OpConstant, 2),
						Definition.make(OpSetLocal, 0),
						Definition.make(OpGetFree, 0),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpClosure, 4, 2),
						Definition.make(OpReturnValue),
					),
					List(
						Definition.make(OpConstant, 1),
						Definition.make(OpSetLocal, 0),
						Definition.make(OpGetLocal, 0),
						Definition.make(OpClosure, 5, 1),
						Definition.make(OpReturnValue),
					),
				),
				List(
					Definition.make(OpConstant, 0),
					Definition.make(OpSetGlobal, 0),
					Definition.make(OpClosure, 6, 0),
					Definition.make(OpPop),
				)
			),
		))
	}
}
