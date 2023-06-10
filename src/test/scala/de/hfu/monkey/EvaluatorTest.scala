package de.hfu.monkey

import de.hfu.monkey.objects.{BooleanObject, IntegerObject, NullObject, StringObject}
import de.hfu.monkey.parser.CombinatorParser
import de.hfu.monkey.evaluator.{Environment, Evaluator}
import org.scalatest.funsuite.AnyFunSuite

class EvaluatorTest extends AnyFunSuite {

	private val parser = CombinatorParser()

	test("evaluator.integer") {
		var parsed = parser.parse("1;")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(1))

		parsed = parser.parse("123;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(123))
	}

	test("evaluator.boolean") {
		var parsed = parser.parse("true;")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == BooleanObject(true))

		parsed = parser.parse("false;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == BooleanObject(false))
	}

	test("evaluator.null") {
		val parsed = parser.parse("let foo = 1;")
		val evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == NullObject)
	}

	test("evaluator.prefixExpression") {
		var parsed = parser.parse("-1;")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(-1))

		parsed = parser.parse("--1;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(1))

		parsed = parser.parse("!true;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == BooleanObject(false))

		parsed = parser.parse("!!false;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == BooleanObject(false))

		parsed = parser.parse("-false;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == NullObject)
	}

	test("evaluator.infixExpression") {
		var parsed = parser.parse("2 + 3;")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(5))

		parsed = parser.parse("2 - 3;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(-1))

		parsed = parser.parse("2 * 3;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(6))

		parsed = parser.parse("2 / 3;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(0))

		parsed = parser.parse("(1 == 1) == true;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == BooleanObject(true))

		parsed = parser.parse("(1 != 1) == false;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == BooleanObject(true))
	}

	test("evaluator.groupedExpression") {
		var parsed = parser.parse("(1 + 2) * 3;")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(9))

		parsed = parser.parse("((1 + 2) * (3 - -5) / 2) + (1 / 1) - 0;")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(13))
	}

	test("evaluator.letStatement") {
		val parsed = parser.parse("let a = 1; let b = a * 2; b + a;")
		val evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(3))
	}

	test("evaluator.ifStatement") {
		val parsed = parser.parse("if (1 > 0) { if (1 < 0) { 1; } else { 2; }; } else { 3; };")
		val evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(2))
	}

	test("evaluator.function") {
		var parsed = parser.parse("let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(6);")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(8))

		parsed = parser.parse("let getOne = fn() { 1; }; getOne();")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(1))

		parsed = parser.parse("let getNull = fn() { }; getNull();")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == NullObject)

		parsed = parser.parse("let getTwo = fn() { 1; return 2; 3; }; getTwo();")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(2))

		parsed = parser.parse("let bar = 1; let foo = fn(x) { let bar = 2; bar + x; }; foo(bar);")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(3))
	}

	test("evaluator.array") {
		var parsed = parser.parse("let foo = [1, \"a\", 1+2, \"b\"]; foo[1];")
		var evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == StringObject("a"))

		parsed = parser.parse("[1, 2, 3][1];")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(2))

		parsed = parser.parse("let a = [[1, 2], 3]; a[0][1] + a[1];")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == IntegerObject(5))

		parsed = parser.parse("[][1];")
		evaluated = Evaluator.evaluate(Some(parsed), new Environment)
		assert(evaluated == NullObject)
	}

}