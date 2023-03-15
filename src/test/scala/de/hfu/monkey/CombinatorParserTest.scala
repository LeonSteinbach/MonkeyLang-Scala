package de.hfu.monkey

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Failure

class CombinatorParserTest extends AnyFunSuite {

  val parser = new CombinatorParser()

  test("parser.identifier") {
    assert(parser.parseAll(parser.program, "foo; a123b;").get ===
      Program(List(
          ExpressionStatement(Identifier("foo")),
          ExpressionStatement(Identifier("a123b")))))

    parser.parseAll(parser.program, "1a;") match {
      case parser.Failure(_, _) =>
      case _ => fail()
    }
  }

  test("parser.integer") {
    assert(parser.parseAll(parser.program, "0; 1; 123;").get ===
      Program(List(
          ExpressionStatement(IntegerLiteral(0)),
          ExpressionStatement(IntegerLiteral(1)),
          ExpressionStatement(IntegerLiteral(123)))))
  }

  test("parser.boolean") {
    assert(parser.parseAll(parser.program, "true; false;").get ===
      Program(List(
          ExpressionStatement(BooleanLiteral(true)),
          ExpressionStatement(BooleanLiteral(false)))))
  }

  test("parser.prefixExpression") {
    assert(parser.parseAll(parser.program, "!true; !!false; -1; --foo;").get ===
      Program(List(
          ExpressionStatement(PrefixExpression("!", BooleanLiteral(true))),
          ExpressionStatement(PrefixExpression("!", PrefixExpression("!", BooleanLiteral(false)))),
          ExpressionStatement(PrefixExpression("-", IntegerLiteral(1))),
          ExpressionStatement(PrefixExpression("-", PrefixExpression("-", Identifier("foo")))))))
  }

  test("parser.infixExpression") {
    assert(parser.parseAll(parser.program, "1 + 2; 1 - 2; 1 * 2; 1 / 2; 1 < 2; 1 > 2; 1 == 2; 1 != 2;").get ===
      Program(List(
          ExpressionStatement(InfixExpression("+", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("-", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("*", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("/", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("<", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression(">", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("==", IntegerLiteral(1), IntegerLiteral(2))),
          ExpressionStatement(InfixExpression("!=", IntegerLiteral(1), IntegerLiteral(2))))))

    assert(parser.parseAll(parser.program, "a + 1; foo() == bar(1, a);").get ===
      Program(List(
          ExpressionStatement(InfixExpression("+", Identifier("a"), IntegerLiteral(1))),
          ExpressionStatement(InfixExpression("==",
            CallExpression(Identifier("foo"), List()),
            CallExpression(Identifier("bar"), List(IntegerLiteral(1), Identifier("a"))))))))
  }

  test("parser.groupedExpression") {
    assert(parser.parseAll(parser.program, "1 + 2 * 3; 1 + (2 * 3); (1 + 2) * 3;").get ===
      Program(List(
        ExpressionStatement(
          InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3)))),
        ExpressionStatement(
          InfixExpression("+", IntegerLiteral(1), InfixExpression("*", IntegerLiteral(2), IntegerLiteral(3)))),
        ExpressionStatement(
          InfixExpression("*", InfixExpression("+", IntegerLiteral(1), IntegerLiteral(2)), IntegerLiteral(3))))))
  }

  test("parser.ifExpression") {
    assert(parser.parseAll(parser.program, "if (a > 0) { a; }; if (a < b) { } else { b; };").get ===
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
    assert(parser.parseAll(parser.program, "fn (a, b) { a + b; }; fn () {};").get ===
      Program(List(
        ExpressionStatement(FunctionLiteral(
          List(Identifier("a"), Identifier("b")),
          BlockStatement(List(ExpressionStatement(InfixExpression("+", Identifier("a"), Identifier("b"))))))),
        ExpressionStatement(FunctionLiteral(
          List(),
          BlockStatement(Nil))))))
  }

  test("parser.callExpression") {
    assert(parser.parseAll(parser.program, "add(1, 2); foo(a + 1, bar());").get ===
      Program(List(
        ExpressionStatement(CallExpression(
          Identifier("add"),
          List(IntegerLiteral(1), IntegerLiteral(2)))),
        ExpressionStatement(CallExpression(
          Identifier("foo"),
          List(InfixExpression("+", Identifier("a"), IntegerLiteral(1)), CallExpression(Identifier("bar"), List())))))))
  }

  test("parser.letStatement") {
    assert(parser.parseAll(parser.program, "let foo = 1; let bar = foo + 2;").get ===
      Program(List(
        LetStatement(Identifier("foo"), IntegerLiteral(1)),
        LetStatement(Identifier("bar"), InfixExpression("+", Identifier("foo"), IntegerLiteral(2))))))
  }

  test("parser.returnStatement") {
    assert(parser.parseAll(parser.program, "return 1; return foo + bar;").get ===
      Program(List(
        ReturnStatement(IntegerLiteral(1)),
        ReturnStatement(InfixExpression("+", Identifier("foo"), Identifier("bar"))))))
  }
}
