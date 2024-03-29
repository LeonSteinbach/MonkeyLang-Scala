package de.hfu.monkey

import de.hfu.monkey.ast.*
import de.hfu.monkey.parser.*
import de.hfu.monkey.objects.*
import de.hfu.monkey.compiler.*
import de.hfu.monkey.vm.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap

class VmTest extends AnyFunSuite {

	case class Test(input: String, expected: Object)

	def testIntegerObject(expected: IntegerObject, actual: Object): Unit = {
		actual match {
			case IntegerObject(value) if value == expected.value =>
			case obj => fail(s"Object does not match expected value. Got $obj, expected Integer with value $expected")
		}
	}

	def testBooleanObject(expected: BooleanObject, actual: Object): Unit = {
		actual match {
			case BooleanObject(value) if value == expected.value =>
			case obj => fail(s"Object does not match expected value. Got $obj, expected Boolean with value $expected")
		}
	}

	def testStringObject(expected: StringObject, actual: Object): Unit = {
		actual match {
			case StringObject(value) if value == expected.value =>
			case obj => fail(s"Object does not match expected value. Got $obj, expected String with value $expected")
		}
	}

	def testArrayObject(expected: ArrayObject, actual: Object): Unit = {
		actual match {
			case array: ArrayObject =>
				if (array.elements.length != expected.elements.length) {
					throw new Exception(s"array has wrong number of elements: expected ${expected.elements.length}, got ${array.elements.length}")
				}
				for ((expectedElem, actualElem) <- expected.elements.zip(array.elements)) {
					testExpectedObject(expectedElem, actualElem)
				}
			case _ => throw new Exception(s"object is not an array: ${actual.`type`()} $actual")
		}
	}

	def testHashObject(expected: HashObject, actual: Object): Unit = {
		actual match {
			case hash: HashObject =>
				if (hash.pairs.size != expected.pairs.size) {
					throw new Exception(s"hash has wrong number of pairs: expected ${expected.pairs.size}, got ${hash.pairs.size}")
				}
				for ((expectedKey, expectedPair) <- expected.pairs) {
					hash.pairs.get(expectedKey) match {
						case Some(actualPair) =>
							testExpectedObject(expectedPair.key, actualPair.key)
							testExpectedObject(expectedPair.value, actualPair.value)
						case None => throw new Exception(s"hash missing expected key: ${expectedKey.value}")
					}
				}
			case _ => throw new Exception(s"object is not a hash: ${actual.`type`()} $actual")
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

			val stackElement = vm.lastPoppedStackElement
			testExpectedObject(test.expected, stackElement)
		}
	}

	def testExpectedObject(expected: Object, actual: Object): Unit = {
		expected match {
			case integer: IntegerObject => testIntegerObject(integer, actual)
			case boolean: BooleanObject => testBooleanObject(boolean, actual)
			case string: StringObject => testStringObject(string, actual)
			case array: ArrayObject => testArrayObject(array, actual)
			case hash: HashObject => testHashObject(hash, actual)
			case NULL =>
				if (actual != NULL)
					throw new Exception(s"object is not null: ${actual.`type`()} $actual")
		}
	}

	test("vm.integerArithmetic") {
		runVmTests(List(
			Test("1;", IntegerObject(1)),
			Test("2;", IntegerObject(2)),
			Test("1 + 2;", IntegerObject(3)),
			Test("1 - 2;", IntegerObject(-1)),
			Test("2 * 3;", IntegerObject(6)),
			Test("6 / 2;", IntegerObject(3)),
			Test("50 / 2 * 2 + 10 - 5;", IntegerObject(55)),
			Test("-1;", IntegerObject(-1)),
			Test("--1;", IntegerObject(1)),
			Test("-50 + 100 + -50;", IntegerObject(0)),
		))
	}

	test("vm.booleanExpression") {
		runVmTests(List(
			Test("true;", BooleanObject(true)),
			Test("false;", BooleanObject(false)),
			Test("1 < 2;", BooleanObject(true)),
			Test("1 > 2;", BooleanObject(false)),
			Test("1 < 1;", BooleanObject(false)),
			Test("1 > 1;", BooleanObject(false)),
			Test("1 == 1;", BooleanObject(true)),
			Test("1 != 1;", BooleanObject(false)),
			Test("1 == 2;", BooleanObject(false)),
			Test("1 != 2;", BooleanObject(true)),
			Test("true == true;", BooleanObject(true)),
			Test("false == false;", BooleanObject(true)),
			Test("true != false;", BooleanObject(true)),
			Test("false != true;", BooleanObject(true)),
			Test("(1 < 2) == true;", BooleanObject(true)),
			Test("(1 < 2) == false;", BooleanObject(false)),
			Test("(1 > 2) == true;", BooleanObject(false)),
			Test("(1 > 2) == false;", BooleanObject(true)),
			Test("!true;", BooleanObject(false)),
			Test("!!true;", BooleanObject(true)),
			Test("!false;", BooleanObject(true)),
			Test("!!false;", BooleanObject(false)),
			Test("!!5;", BooleanObject(true)),
			Test("!(if (false) { 5; });", BooleanObject(true)),
		))
	}

	test("vm.conditionals") {
		runVmTests(List(
			Test("if (true) { 10; };", IntegerObject(10)),
			Test("if (true) { 10; } else { 20; };", IntegerObject(10)),
			Test("if (false) { 10; } else { 20; };", IntegerObject(20)),
			Test("if (1) { 10; };", IntegerObject(10)),
			Test("if (1 < 2) { 10; };", IntegerObject(10)),
			Test("if (1 < 2) { 10; } else { 20; };", IntegerObject(10)),
			Test("if (1 > 2) { 10; } else { 20; };", IntegerObject(20)),
			Test("if (1 > 2) { 10; };", NULL),
			Test("if ((if (false) { 10; })) { 10; } else { 20; };", IntegerObject(20)),
		))
	}

	test("vm.globalLetStatements") {
		runVmTests(List(
			Test("let one = 1; one;", IntegerObject(1)),
			Test("let one = 1; let two = 2; one + two;", IntegerObject(3)),
			Test("let one = 1; let two = one + one; one + two;", IntegerObject(3)),
		))
	}

	test("vm.stringExpressions") {
		runVmTests(List(
			Test("\"monkey\";", StringObject("monkey")),
			Test("\"mon\" + \"key\";", StringObject("monkey")),
			Test("\"mon\" + \"key\" + \" banana\";", StringObject("monkey banana")),
		))
	}

	test("vm.arrayLiterals") {
		runVmTests(List(
			Test("[];", ArrayObject(List.empty)),
			Test("[1, 2, 3];", ArrayObject(List(IntegerObject(1), IntegerObject(2), IntegerObject(3)))),
			Test("[true, false];", ArrayObject(List(BooleanObject(true), BooleanObject(false)))),
			Test("[1, \"hello\", [2, 3]];", ArrayObject(List(IntegerObject(1), StringObject("hello"), ArrayObject(List(IntegerObject(2), IntegerObject(3)))))),
		))
	}

	test("vm.hashLiterals") {
		runVmTests(List(
			Test("{};", HashObject(Map.empty)),
			Test("{1: 2};", HashObject(Map(IntegerObject(1).hashKey -> HashPair(IntegerObject(1), IntegerObject(2))))),
			Test("{1: 2, 3: 4};", HashObject(Map(
				IntegerObject(1).hashKey -> HashPair(IntegerObject(1), IntegerObject(2)),
				IntegerObject(3).hashKey -> HashPair(IntegerObject(3), IntegerObject(4))))),
			Test("{true: false};", HashObject(Map(BooleanObject(true).hashKey -> HashPair(BooleanObject(true), BooleanObject(false))))),
			Test("{1: 2, \"foo\": \"bar\", true: [3, 4]};", HashObject(Map(
				IntegerObject(1).hashKey -> HashPair(IntegerObject(1), IntegerObject(2)),
				StringObject("foo").hashKey -> HashPair(StringObject("foo"), StringObject("bar")),
				BooleanObject(true).hashKey -> HashPair(BooleanObject(true), ArrayObject(List(IntegerObject(3), IntegerObject(4))))))),
		))
	}

	test("vm.indexExpressions") {
		runVmTests(List(
			Test("[1, 2, 3][1];", IntegerObject(2)),
			Test("[1, 2, 3][0 + 2];", IntegerObject(3)),
			Test("[[1, 2, 3], 4][0][1];", IntegerObject(2)),
			Test("[][0];", NULL),
			Test("[1][-1];", NULL),
			Test("{1: 2, 3: 4}[1];", IntegerObject(2)),
			Test("{1: 2, 3: 4}[3];", IntegerObject(4)),
			Test("{\"a\": 1, \"b\": 2}[\"a\"];", IntegerObject(1)),
			Test("{1: 2, 3: 4}[2];", NULL),
			Test("{}[0];", NULL),
		))
	}

	test("vm.functionCallsWithoutArguments") {
		runVmTests(List(
			Test("let three = fn() { 1 + 2; }; three();", IntegerObject(3)),
			Test("let foo = fn() { 1; }; let bar = fn() { 2; }; foo() + bar();", IntegerObject(3)),
			Test("let foo = fn() { 1; }; let bar = fn() { foo() + 2; }; bar();", IntegerObject(3)),
		))
	}

	test("vm.functionCallsWithReturnStatement") {
		runVmTests(List(
			Test("let foo = fn() { return 1; 2; 3; }; foo();", IntegerObject(1)),
			Test("let foo = fn() { return 1; return 2; }; foo();", IntegerObject(1)),
		))
	}

	test("vm.functionCallsWithoutReturnValue") {
		runVmTests(List(
			Test("let foo = fn() { }; foo();", NULL),
			Test("let foo = fn() { }; let bar = fn() { foo(); }; bar();", NULL),
		))
	}

	test("vm.firstClassFunctions") {
		runVmTests(List(
			Test("let foo = fn() { 1; }; let bar = fn() { foo; }; bar()();", IntegerObject(1)),
		))
	}

	test("vm.functionCallsWithBindings") {
		runVmTests(List(
			Test("let foo = fn() { let bar = 1; bar; }; foo();", IntegerObject(1)),
			Test("let foo = fn() { let one = 1; let two = 2; one + two; }; foo();", IntegerObject(3)),
			Test("let foo = fn() { let one = 1; let two = 2; one + two; }; let bar = fn() { let three = 3; let four = 4; three + four; }; foo() + bar();", IntegerObject(10)),
			Test("let first = fn() { let bar = 1; bar; }; let second = fn() { let bar = 2; bar; }; first() + second();", IntegerObject(3)),
			Test("let global = 50; let foo = fn () { let num = 1; global - num; }; let bar = fn () { let num = 2; global - num; }; foo() + bar();", IntegerObject(97)),
		))
	}

	test("vm.functionCallsWithArgumentsAndBindings") {
		runVmTests(List(
			Test("let foo = fn(a) { a; }; foo(1);", IntegerObject(1)),
			Test("let sum = fn(a, b) { a + b; }; sum(1, 2);", IntegerObject(3)),
			Test("let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2);", IntegerObject(3)),
			Test("let sum = fn(a, b) { let c = a + b; c; }; sum(1, 2) + sum(3, 4);", IntegerObject(10)),
			Test("let sum = fn(a, b) { let c = a + b; c; }; let outer = fn() { sum(1, 2) + sum(3, 4); }; outer();", IntegerObject(10)),
			Test("let global = 10; let sum = fn(a, b) { let c = a + b; c + global; }; let outer = fn() { sum(1, 2) + sum(3, 4) + global; }; outer() + global;", IntegerObject(50)),
		))
	}

	test("vm.functionCallsWithWrongArguments") {
		case class ErrorTest(input: String, expected: String)

		val tests: List[ErrorTest] = List(
			ErrorTest(
				"fn() { 1; }(1);",
				"wrong number of arguments: want=0, got=1",
			),
			ErrorTest(
				"fn(a) { a; }();",
				"wrong number of arguments: want=1, got=0",
			),
			ErrorTest(
				"fn(a, b) { a + b; }(1);",
				"wrong number of arguments: want=2, got=1",
			),
		)

		tests.foreach { case ErrorTest(input, expected) =>
			val program = ManualParser().parse(input)
			val compiler = Compiler()
			compiler.compile(program)

			val vm = Vm(compiler.bytecode)

			try {
				vm.run()
				fail("expected VM error but resulted in none.")
			} catch {
				case e: Exception => assert(expected == e.getMessage)
			}
		}
	}

	test("vm.closures") {
		runVmTests(List(
			Test("let newClosure = fn(a) { fn() { a; }; }; let closure = newClosure(99); closure();", IntegerObject(99)),
			Test("let newAdder = fn(a, b) { fn(c) { a + b + c; }; }; let adder = newAdder(1, 2); adder(8);", IntegerObject(11)),
			Test("let newAdder = fn(a, b) { let c = a + b; fn(d) { c + d; }; }; let adder = newAdder(1, 2); adder(8);", IntegerObject(11)),
		))
	}

	test("vm.recursiveFunctions") {
		runVmTests(List(
			Test("let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); }; }; countDown(1);", IntegerObject(0)),
			Test("let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); }; }; let wrapper = fn() { countDown(1); }; wrapper();", IntegerObject(0)),
			Test("let wrapper = fn() { let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); }; }; countDown(1); }; wrapper();", IntegerObject(0)),
		))
	}

	test("vm.builtins") {
		runVmTests(List(
			Test("len(\"\");", IntegerObject(0)),
			Test("len(\"four\");", IntegerObject(4)),
			Test("len(\"hello world\");", IntegerObject(11)),
			Test("len([1, 2, 3]);", IntegerObject(3)),
			Test("puts(\"hello\");", NullObject),
			Test("first([1, 2, 3]);", IntegerObject(1)),
			Test("first([]);", NullObject),
			Test("last([1, 2, 3]);", IntegerObject(3)),
			Test("last([]);", NullObject),
			Test("rest([1, 2, 3]);", ArrayObject(List(IntegerObject(2), IntegerObject(3)))),
			Test("rest([]);", NullObject),
			Test("push([], 1);", ArrayObject(List(IntegerObject(1)))),
		))
	}

}
