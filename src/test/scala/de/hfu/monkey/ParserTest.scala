package de.hfu.monkey

import de.hfu.monkey.ast.{ArrayLiteral, BlockStatement, BooleanLiteral, CallExpression, ExpressionStatement, FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression, IntegerLiteral, LetStatement, PrefixExpression, Program, ReturnStatement, StringLiteral}
import de.hfu.monkey.parser.{CombinatorParser, ManualParser}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Failure

class ParserTest extends AnyFunSuite {

	private val parserCombinator: CombinatorParser = CombinatorParser()
	private val parserManual: ManualParser = ManualParser()

	test("parser.identifier") {
		val program: Program = parserCombinator.parse("foo; a123b;")
		assert(parserManual.parse("foo; a123b;") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(Identifier("foo")),
				ExpressionStatement(Identifier("a123b")))))
	}

	test("parser.integer") {
		val program: Program = parserCombinator.parse("0; 1; 123;")
		assert(parserManual.parse("0; 1; 123;") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(IntegerLiteral(0)),
				ExpressionStatement(IntegerLiteral(1)),
				ExpressionStatement(IntegerLiteral(123)))))
	}

	test("parser.boolean") {
		val program: Program = parserCombinator.parse("true; false;")
		assert(parserManual.parse("true; false;") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(BooleanLiteral(true)),
				ExpressionStatement(BooleanLiteral(false)))))
	}

	test("parser.string") {
		val program: Program = parserCombinator.parse("\"hello\"; \"\";")
		assert(parserManual.parse("\"hello\"; \"\";") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(StringLiteral("hello")),
				ExpressionStatement(StringLiteral("")))))
	}

	test("parser.array") {
		val program: Program = parserCombinator.parse("[0, \"a\", !true, foo]; [];")
		assert(parserManual.parse("[0, \"a\", !true, foo]; [];") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(ArrayLiteral(List(IntegerLiteral(0), StringLiteral("a"), PrefixExpression("!", BooleanLiteral(true)), Identifier("foo")))),
				ExpressionStatement(ArrayLiteral(List())))))
	}

	test("parser.hash") {
		val program: Program = parserCombinator.parse("{ \"a\": 1, 2: \"b\", 3 + 4: 5, true: false }; {};")
		assert(parserManual.parse("{ \"a\": 1, 2: \"b\", 3 + 4: 5, true: false }; {};") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(HashLiteral(Map(
					StringLiteral("a") -> IntegerLiteral(1),
					IntegerLiteral(2) -> StringLiteral("b"),
					InfixExpression("+", IntegerLiteral(3), IntegerLiteral(4)) -> IntegerLiteral(5),
					BooleanLiteral(true) -> BooleanLiteral(false)))),
				ExpressionStatement(HashLiteral(Map())))))
	}

	test("parser.prefixExpression") {
		val program: Program = parserCombinator.parse("!true; !!false; -1; --foo; !(true); -(if (true) { 1; });")
		assert(parserManual.parse("!true; !!false; -1; --foo; !(true); -(if (true) { 1; });") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(PrefixExpression("!", BooleanLiteral(true))),
				ExpressionStatement(PrefixExpression("!", PrefixExpression("!", BooleanLiteral(false)))),
				ExpressionStatement(PrefixExpression("-", IntegerLiteral(1))),
				ExpressionStatement(PrefixExpression("-", PrefixExpression("-", Identifier("foo")))),
				ExpressionStatement(PrefixExpression("!", BooleanLiteral(true))),
				ExpressionStatement(PrefixExpression("-", IfExpression(BooleanLiteral(true), BlockStatement(List(ExpressionStatement(IntegerLiteral(1)))), BlockStatement(List())))))))
	}

	test("parser.infixExpression") {
		var program: Program = parserCombinator.parse("1 + 2; 1 - 2; 1 * 2; 1 / 2; 1 < 2; 1 > 2; 1 == 2; 1 != 2;")
		assert(parserManual.parse("1 + 2; 1 - 2; 1 * 2; 1 / 2; 1 < 2; 1 > 2; 1 == 2; 1 != 2;") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(InfixExpression("+", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression("-", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression("*", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression("/", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression("<", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression(">", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression("==", IntegerLiteral(1), IntegerLiteral(2))),
				ExpressionStatement(InfixExpression("!=", IntegerLiteral(1), IntegerLiteral(2))))))

		program = parserCombinator.parse("a + 1; foo() == bar(1, a);")
		assert(parserManual.parse("a + 1; foo() == bar(1, a);") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(InfixExpression("+", Identifier("a"), IntegerLiteral(1))),
				ExpressionStatement(InfixExpression("==",
					CallExpression(Identifier("foo"), List()),
					CallExpression(Identifier("bar"), List(IntegerLiteral(1), Identifier("a"))))))))
	}

	test("parser.groupedExpression") {
		val program: Program = parserCombinator.parse("1 + 2 * 3; 1 + (2 * 3); (1 + 2) * 3;")
		assert(parserManual.parse("1 + 2 * 3; 1 + (2 * 3); (1 + 2) * 3;") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(
					InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3)))),
				ExpressionStatement(
					InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3)))),
				ExpressionStatement(
					InfixExpression("*", InfixExpression("+", IntegerLiteral(1), IntegerLiteral(2)), IntegerLiteral(3))))))
	}

	test("parser.ifExpression") {
		val program: Program = parserCombinator.parse("if (a > 0) { a; }; if (a < b) { } else { b; };")
		assert(parserManual.parse("if (a > 0) { a; }; if (a < b) { } else { b; };") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(IfExpression(
					InfixExpression(">", Identifier("a"), IntegerLiteral(0)),
					BlockStatement(List(ExpressionStatement(Identifier("a")))),
					BlockStatement(List()))),
				ExpressionStatement(IfExpression(
					InfixExpression("<", Identifier("a"), Identifier("b")),
					BlockStatement(List()),
					BlockStatement(List(ExpressionStatement(Identifier("b")))))))))
	}

	test("parser.functionLiteral") {
		val program: Program = parserCombinator.parse("fn (a, b) { a + b; }; fn () {};")
		assert(parserManual.parse("fn (a, b) { a + b; }; fn () {};") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(FunctionLiteral(
					List(Identifier("a"), Identifier("b")),
					BlockStatement(List(ExpressionStatement(InfixExpression("+", Identifier("a"), Identifier("b"))))))),
				ExpressionStatement(FunctionLiteral(
					List(),
					BlockStatement(Nil))))))
	}

	test("parser.callExpression") {
		val program: Program = parserCombinator.parse("add(1, 2); foo(a + 1, bar());")
		assert(parserManual.parse("add(1, 2); foo(a + 1, bar());") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(CallExpression(
					Identifier("add"),
					List(IntegerLiteral(1), IntegerLiteral(2)))),
				ExpressionStatement(CallExpression(
					Identifier("foo"),
					List(InfixExpression("+", Identifier("a"), IntegerLiteral(1)), CallExpression(Identifier("bar"), List())))))))
	}

	test("parser.firstClassFunctions") {
		val program: Program = parserCombinator.parse("foo()();")
		assert(parserManual.parse("foo()();") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(CallExpression(CallExpression(Identifier("foo"), List()), List())))))
	}

	test("parser.indexExpression") {
		val program: Program = parserCombinator.parse("foo[-1]; [2][0]; (foo + bar)[3]; a[0][1] + b[0]; {\"a\": 1}[\"a\"];")
		assert(parserManual.parse("foo[-1]; [2][0]; (foo + bar)[3]; a[0][1] + b[0]; {\"a\": 1}[\"a\"];") === program)
		assert(program ===
			Program(List(
				ExpressionStatement(IndexExpression(Identifier("foo"),PrefixExpression("-",IntegerLiteral(1)))),
				ExpressionStatement(IndexExpression(ArrayLiteral(List(IntegerLiteral(2))),IntegerLiteral(0))),
				ExpressionStatement(IndexExpression(InfixExpression("+",Identifier("foo"),Identifier("bar")),IntegerLiteral(3))),
				ExpressionStatement(InfixExpression("+",IndexExpression(IndexExpression(Identifier("a"),IntegerLiteral(0)),IntegerLiteral(1)),IndexExpression(Identifier("b"),IntegerLiteral(0)))),
				ExpressionStatement(IndexExpression(HashLiteral(Map(StringLiteral("a") -> IntegerLiteral(1))), StringLiteral("a"))))))
	}

	test("parser.letStatement") {
		val program: Program = parserCombinator.parse("let foo = 1; let bar = foo + 2;")
		assert(parserManual.parse("let foo = 1; let bar = foo + 2;") === program)
		assert(program ===
			Program(List(
				LetStatement(Identifier("foo"), IntegerLiteral(1)),
				LetStatement(Identifier("bar"), InfixExpression("+", Identifier("foo"), IntegerLiteral(2))))))
	}

	test("parser.returnStatement") {
		val program: Program = parserCombinator.parse("return 1; return foo + bar;")
		assert(parserManual.parse("return 1; return foo + bar;") === program)
		assert(program ===
			Program(List(
				ReturnStatement(IntegerLiteral(1)),
				ReturnStatement(InfixExpression("+", Identifier("foo"), Identifier("bar"))))))
	}

	test("parser.precedences") {
		val input: String = "1 + 2 * 3 < 4 == 5 - 6 / 7; myFunc(8); myArray[9 + 10]; !-11; foo()[12]; foo[13]();"
		val program: Program = parserCombinator.parse(input)
		assert(parserManual.parse(input) === program)
		assert(program === Program(List(
			ExpressionStatement(
				InfixExpression("==",
					InfixExpression("<",
						InfixExpression("+",
							IntegerLiteral(1),
							InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3))),
						IntegerLiteral(4)),
					InfixExpression("-",
						IntegerLiteral(5),
						InfixExpression("/", IntegerLiteral(6), IntegerLiteral(7))
					)
				)
			),
			ExpressionStatement(
				CallExpression(Identifier("myFunc"), List(IntegerLiteral(8)))
			),
			ExpressionStatement(
				IndexExpression(Identifier("myArray"),
					InfixExpression("+", IntegerLiteral(9), IntegerLiteral(10))
				)
			),
			ExpressionStatement(
				PrefixExpression("!", PrefixExpression("-", IntegerLiteral(11)))
			),
			ExpressionStatement(
				IndexExpression(
					CallExpression(Identifier("foo"), List()), IntegerLiteral(12)
				)
			),
			ExpressionStatement(
				CallExpression(
					IndexExpression(Identifier("foo"), IntegerLiteral(13)),
					List()
				)
			)
		)))
	}

	test("parser.functionLiteralWithName") {
		val program: Program = parserCombinator.parse("let myFunction = fn() {};")
		assert(parserManual.parse("let myFunction = fn() {};") === program)

		if (program.statements.length != 1)
			fail(s"program body does not contain 1 statement. got=${program.statements.length}")

		program.statements.head match {
			case letStatement: LetStatement =>
				letStatement.value match {
					case functionLiteral: FunctionLiteral =>
						if (functionLiteral.name != "myFunction")
							fail(s"function literal name wrong. want='myFunction', got=${functionLiteral.name}")
					case other => fail(s"value of letStatement is not a functionLiteral. got=$other")
				}
			case other => fail(s"program statements at 0 is not a letStatement. got=$other")
		}
	}
}
