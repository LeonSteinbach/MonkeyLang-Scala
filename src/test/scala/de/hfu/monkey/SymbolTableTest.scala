package de.hfu.monkey

import de.hfu.monkey.compiler.*
import de.hfu.monkey.compiler.SymbolScope.*
import org.scalatest.funsuite.AnyFunSuite

class SymbolTableTest extends AnyFunSuite {

	test("symbolTable.define") {
		val expected: Map[String, Symbol] = Map(
			"a" -> Symbol("a", GLOBAL, 0),
			"b" -> Symbol("b", GLOBAL, 1),
		)

		val global = new SymbolTable()

		val a = global.define("a")
		if (a != expected("a"))
			fail(s"expected a=${expected("a")} got=$a")

		val b = global.define("b")
		if (b != expected("b"))
			fail(s"expected b=${expected("b")} got=$b")
	}

	test("symbolTable.resolve") {
		val global = new SymbolTable()
		global.define("a")
		global.define("b")

		val expected: List[Symbol] = List(
			Symbol("a", GLOBAL, 0),
			Symbol("b", GLOBAL, 1),
		)

		expected.foreach { symbol =>
			val result = global.resolve(symbol.name)
			if (result != symbol)
				fail(s"expected ${symbol.name} to resolve to $symbol, got=$result")
		}
	}

}
