package de.hfu.monkey

import de.hfu.monkey.ast.*
import de.hfu.monkey.parser.*
import de.hfu.monkey.objects.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.vm.*
import org.scalatest.funsuite.AnyFunSuite

class VmTest extends AnyFunSuite {

	case class Test(input: String, expected: Any)

	def testIntegerObject(expected: Int, actual: Object): Unit = {
		actual match {
			case IntegerObject(value) if value == expected =>
			case obj => fail(s"Object does not match expected value. Got $obj, expected Integer with value $expected")
		}
	}

	def runVmTests(tests: List[Test]): Unit = {
		val parser = CombinatorParser()
		for (test <- tests) {
			val program = parser.parse(test.input)

			val compiler = Compiler()
			compiler.compile(program)

			val vm = Vm(compiler.bytecode)
			vm.run()

			vm.stackTop match {
				case Some(value: Object) => testExpectedObject(test.expected, value)
				case None => fail("empty stack")
			}
		}
	}

	def testExpectedObject(expected: Any, actual: Object): Unit = {
		expected match {
			case integer: Int => testIntegerObject(integer, actual)
		}
	}

	test("vm.integerArithmetic") {
		runVmTests(List(
			Test("1;", 1),
			Test("2;", 2),
			Test("1 + 2;", 3)
		))
	}

}
