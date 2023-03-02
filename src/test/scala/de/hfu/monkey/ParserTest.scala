package de.hfu.monkey

import org.scalatest.funsuite.AnyFunSuite
import de.hfu.monkey.MonkeyParser

import scala.util.Failure

class ParserTest extends AnyFunSuite {

  val parser = new MonkeyParser()

  test("parser.identifier") {

    assert(parser.parseAll(parser.program, "foo; a123b;").get ===
      Program(
        List(
          ExpressionStatement(
            Identifier("foo")),
          ExpressionStatement(
            Identifier("a123b")))))

    parser.parseAll(parser.program, "1a;") match {
      case parser.Failure(msg, _) =>
      case _ => fail()
    }

  }
}
