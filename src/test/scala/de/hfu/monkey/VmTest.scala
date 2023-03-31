package de.hfu.monkey

import de.hfu.monkey.ast.*
import de.hfu.monkey.evaluator.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.parser.*
import de.hfu.monkey.vm.*
import org.scalatest.funsuite.AnyFunSuite

class VmTest extends AnyFunSuite {

	case class Test(input: String, expected: Any)

	def testIntegerObject(expected: Int, actual: Object): Option[Exception] = {
		actual match {
			case IntegerObject(value) if value == expected => None
			case obj => Some(new Exception(s"Object does not match expected value. Got $obj, expected Integer with value $expected"))
		}
	}

	def runVmTests(tests: List[Test]): Unit = {
		val parser = CombinatorParser()
		for (test <- tests) {
			val program = parser.parse(test.input)
			println(program)

			val compiler = Compiler()
			compiler.compile(program) match {
				case Some(exception: Exception) => fail(exception)
				case None =>
			}

			val vm = Vm(compiler.bytecode)
			vm.run() match {
				case Some(exception: Exception) => fail(exception)
				case None =>
			}

			testExpectedObject(test.expected, vm.stackTop)
		}
	}

	def testExpectedObject(expected: Any, actual: Object): Unit = {
		expected match {
			case integer: Int =>
				testIntegerObject(integer, actual) match {
					case Some(exception: Exception) => fail(s"testIntegerObject failed: $exception")
					case None =>
				}
		}
	}

	test("vm.integerArithmetic") {
		runVmTests(List(
			Test("1;", 1)
		))
	}

}
