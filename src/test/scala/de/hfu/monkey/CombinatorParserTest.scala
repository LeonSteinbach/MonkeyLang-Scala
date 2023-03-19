package de.hfu.monkey

import de.hfu.monkey.Parser.CombinatorParser
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Failure

class CombinatorParserTest extends AnyFunSuite {

  private val parser: CombinatorParser = CombinatorParser()

  test("parser.identifier") {
    assert(parser.parse("foo; a123b;") ===
      Program(List(
          ExpressionStatement(Identifier("foo")),
          ExpressionStatement(Identifier("a123b")))))
  }

  test("parser.integer") {
    assert(parser.parse("0; 1; 123;") ===
      Program(List(
          ExpressionStatement(IntegerLiteral(0)),
          ExpressionStatement(IntegerLiteral(1)),
          ExpressionStatement(IntegerLiteral(123)))))
  }

  test("parser.boolean") {
    assert(parser.parse("true; false;") ===
      Program(List(
          ExpressionStatement(BooleanLiteral(true)),
          ExpressionStatement(BooleanLiteral(false)))))
  }

    test("parser.string") {
        assert(parser.parse("\"hello\"; \"\";") ===
            Program(List(
                ExpressionStatement(StringLiteral("hello")),
                ExpressionStatement(StringLiteral("")))))
    }

    test("parser.array") {
        assert(parser.parse("[0, \"a\", !true, foo]; [];") ===
            Program(List(
                ExpressionStatement(ArrayLiteral(List(IntegerLiteral(0), StringLiteral("a"), PrefixExpression("!", BooleanLiteral(true)), Identifier("foo")))),
                ExpressionStatement(ArrayLiteral(List())))))
    }

  test("parser.prefixExpression") {
    assert(parser.parse("!true; !!false; -1; --foo;") ===
      Program(List(
          ExpressionStatement(PrefixExpression("!", BooleanLiteral(true))),
          ExpressionStatement(PrefixExpression("!", PrefixExpression("!", BooleanLiteral(false)))),
          ExpressionStatement(PrefixExpression("-", IntegerLiteral(1))),
          ExpressionStatement(PrefixExpression("-", PrefixExpression("-", Identifier("foo")))))))
  }

  test("parser.infixExpression") {
    assert(parser.parse("1 + 2; 1 - 2; 1 * 2; 1 / 2; 1 < 2; 1 > 2; 1 == 2; 1 != 2;") ===
      Program(List(
          ExpressionStatement(InfixExpression("+", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("-", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("*", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("/", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("<", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression(">", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("==", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("!=", IntegerLiteral(1), IntegerLiteral(2))))))

    assert(parser.parse("a + 1; foo() == bar(1, a);") ===
      Program(List(
          ExpressionStatement(InfixExpression("+", Identifier("a"), IntegerLiteral(1))),
          ExpressionStatement(InfixExpression("==",
            CallExpression(Identifier("foo"), List()),
            CallExpression(Identifier("bar"), List(IntegerLiteral(1), Identifier("a"))))))))
  }

  test("parser.groupedExpression") {
    assert(parser.parse("1 + 2 * 3; 1 + (2 * 3); (1 + 2) * 3;") ===
      Program(List(
        ExpressionStatement(
          InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3)))),
        ExpressionStatement(
          InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3)))),
        ExpressionStatement(
          InfixExpression("*", InfixExpression("+", IntegerLiteral(1), IntegerLiteral(2)), IntegerLiteral(3))))))
  }

  test("parser.ifExpression") {
    assert(parser.parse("if (a > 0) { a; }; if (a < b) { } else { b; };") ===
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
    assert(parser.parse("fn (a, b) { a + b; }; fn () {};") ===
      Program(List(
        ExpressionStatement(FunctionLiteral(
          List(Identifier("a"), Identifier("b")),
          BlockStatement(List(ExpressionStatement(InfixExpression("+", Identifier("a"), Identifier("b"))))))),
        ExpressionStatement(FunctionLiteral(
          List(),
          BlockStatement(Nil))))))
  }

  test("parser.callExpression") {
    assert(parser.parse("add(1, 2); foo(a + 1, bar());") ===
      Program(List(
        ExpressionStatement(CallExpression(
          Identifier("add"),
          List(IntegerLiteral(1), IntegerLiteral(2)))),
        ExpressionStatement(CallExpression(
          Identifier("foo"),
          List(InfixExpression("+", Identifier("a"), IntegerLiteral(1)), CallExpression(Identifier("bar"), List())))))))
  }

  test("parser.letStatement") {
    assert(parser.parse("let foo = 1; let bar = foo + 2;") ===
      Program(List(
        LetStatement(Identifier("foo"), IntegerLiteral(1)),
        LetStatement(Identifier("bar"), InfixExpression("+", Identifier("foo"), IntegerLiteral(2))))))
  }

  test("parser.returnStatement") {
    assert(parser.parse("return 1; return foo + bar;") ===
      Program(List(
        ReturnStatement(IntegerLiteral(1)),
        ReturnStatement(InfixExpression("+", Identifier("foo"), Identifier("bar"))))))
  }
}
