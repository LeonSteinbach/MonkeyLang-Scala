package de.hfu.monkey

import de.hfu.monkey.compiler.*
import de.hfu.monkey.compiler.SymbolScope.*
import org.scalatest.funsuite.AnyFunSuite

class SymbolTableTest extends AnyFunSuite {

	test("symbolTable.define") {
		val expected: Map[String, Symbol] = Map(
			"a" -> Symbol("a", GLOBAL, 0),
			"b" -> Symbol("b", GLOBAL, 1),
			"c" -> Symbol("c", LOCAL, 0),
			"d" -> Symbol("d", LOCAL, 1),
			"e" -> Symbol("e", LOCAL, 0),
			"f" -> Symbol("f", LOCAL, 1),
		)

		val global = new SymbolTable()

		val a = global.define("a")
		if (a != expected("a"))
			fail(s"expected a=${expected("a")} got=$a")

		val b = global.define("b")
		if (b != expected("b"))
			fail(s"expected b=${expected("b")} got=$b")

		val local1 = new SymbolTable(outer = Some(global))

		val c = local1.define("c")
		if (c != expected("c"))
			fail(s"expected c=${expected("c")} got=$c")

		val d = local1.define("d")
		if (d != expected("d"))
			fail(s"expected d=${expected("d")} got=$d")

		val local2 = new SymbolTable(outer = Some(local1))

		val e = local1.define("e")
		if (e != expected("e"))
			fail(s"expected e=${expected("e")} got=$e")

		val f = local1.define("f")
		if (f != expected("f"))
			fail(s"expected f=${expected("f")} got=$f")
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

	test("symbolTable.resolveLocal") {
		val global = new SymbolTable()
		global.define("a")
		global.define("b")

		val local = new SymbolTable(outer = Some(global))
		global.define("c")
		global.define("d")

		val expected: List[Symbol] = List(
			Symbol("a", GLOBAL, 0),
			Symbol("b", GLOBAL, 1),
			Symbol("c", LOCAL, 0),
			Symbol("d", LOCAL, 1),
		)

		expected.foreach { symbol =>
			val result = global.resolve(symbol.name)
			if (result != symbol)
				fail(s"expected ${symbol.name} to resolve to $symbol, got=$result")
		}
	}

}