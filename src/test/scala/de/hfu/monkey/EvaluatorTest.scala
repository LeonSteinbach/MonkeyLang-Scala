package de.hfu.monkey

import org.scalatest.funsuite.AnyFunSuite

class EvaluatorTest extends AnyFunSuite {

	private val parser = new CombinatorParser()

	test("evaluator.integer") {
		var parsed = parser.parseAll(parser.program, "1;")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(1))

		parsed = parser.parseAll(parser.program, "0123;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(123))
	}

	test("evaluator.boolean") {
		var parsed = parser.parseAll(parser.program, "true;")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == BooleanObject(true))

		parsed = parser.parseAll(parser.program, "false;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == BooleanObject(false))
	}

	test("evaluator.null") {
		var parsed = parser.parseAll(parser.program, "let foo = 1;")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == NullObject)
	}

	test("evaluator.prefixExpression") {
		var parsed = parser.parseAll(parser.program, "-1;")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(-1))

		parsed = parser.parseAll(parser.program, "--1;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(1))

		parsed = parser.parseAll(parser.program, "!true;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == BooleanObject(false))

		parsed = parser.parseAll(parser.program, "!!false;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == BooleanObject(false))

		parsed = parser.parseAll(parser.program, "-false;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == NullObject)
	}

	test("evaluator.infixExpression") {
		var parsed = parser.parseAll(parser.program, "2 + 3;")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(5))

		parsed = parser.parseAll(parser.program, "2 - 3;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(-1))

		parsed = parser.parseAll(parser.program, "2 * 3;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(6))

		parsed = parser.parseAll(parser.program, "2 / 3;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(0))

		parsed = parser.parseAll(parser.program, "(1 == 1) == true;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == BooleanObject(true))

		parsed = parser.parseAll(parser.program, "(1 != 1) == false;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == BooleanObject(true))
	}

	test("evaluator.groupedExpression") {
		var parsed = parser.parseAll(parser.program, "(1 + 2) * 3;")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(9))

		parsed = parser.parseAll(parser.program, "((1 + 2) * (3 - -5) / 2) + (1 / 1) - 0;")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(13))
	}

	test("evaluator.letStatement") {
		val parsed = parser.parseAll(parser.program, "let a = 1; let b = a * 2; b + a;")
		val evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(3))
	}

	test("evaluator.ifStatement") {
		val parsed = parser.parseAll(parser.program, "if (1 > 0) { if (1 < 0) { 1; } else { 2; }; } else { 3; };")
		val evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(2))
	}

	test("evaluator.function") {
		var parsed = parser.parseAll(parser.program, "let fib = fn(n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); }; fib(6);")
		var evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(8))

		parsed = parser.parseAll(parser.program, "let getOne = fn() { 1; }; getOne();")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(1))

		parsed = parser.parseAll(parser.program, "let getNull = fn() { }; getNull();")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == NullObject)

		parsed = parser.parseAll(parser.program, "let getTwo = fn() { 1; return 2; 3; }; getTwo();")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(2))

		parsed = parser.parseAll(parser.program, "let bar = 1; let foo = fn(x) { let bar = 2; bar + x; }; foo(bar);")
		evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
		assert(evaluated == IntegerObject(3))
	}

}