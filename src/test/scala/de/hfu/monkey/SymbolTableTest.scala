package de.hfu.monkey

import de.hfu.monkey.compiler.*
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

		val local2 = new SymbolTable(outer = Some(global))

		val e = local2.define("e")
		if (e != expected("e"))
			fail(s"expected e=${expected("e")} got=$e")

		val f = local2.define("f")
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
		local.define("c")
		local.define("d")

		val expected: List[Symbol] = List(
			Symbol("a", GLOBAL, 0),
			Symbol("b", GLOBAL, 1),
			Symbol("c", LOCAL, 0),
			Symbol("d", LOCAL, 1),
		)

		expected.foreach { symbol =>
			val result = symbol.scope match {
				case GLOBAL => global.resolve(symbol.name)
				case LOCAL => local.resolve(symbol.name)
				case _ => fail("free not expected in this test")
			}
			if (result != symbol)
				fail(s"expected ${symbol.name} to resolve to $symbol, got=$result")
		}
	}

	test("symbolTable.resolveFree") {
		val global = new SymbolTable()
		global.define("a")
		global.define("b")

		val local1 = new SymbolTable(outer = Some(global))
		local1.define("c")
		local1.define("d")

		val local2 = new SymbolTable(outer = Some(local1))
		local2.define("e")
		local2.define("f")

		val tests = List(
			(
				local1,
				List(
					Symbol("a", GLOBAL, 0),
					Symbol("b", GLOBAL, 1),
					Symbol("c", LOCAL, 0),
					Symbol("d", LOCAL, 1),
				),
				List.empty
			),
			(
				local2,
				List(
					Symbol("a", GLOBAL, 0),
					Symbol("b", GLOBAL, 1),
					Symbol("c", FREE, 0),
					Symbol("d", FREE, 1),
					Symbol("e", LOCAL, 0),
					Symbol("f", LOCAL, 1),
				),
				List(
					Symbol("c", LOCAL, 0),
					Symbol("d", LOCAL, 1),
				)
			)
		)

		tests.foreach {
			case (table, expectedSymbols, expectedFreeSymbols) =>
				expectedSymbols.foreach { symbol =>
					if (table.resolve(symbol.name) != symbol)
						fail(s"name ${symbol.name} not resolvable")
				}

				if (table.freeSymbols.length != expectedFreeSymbols.length)
					fail(s"wrong number of free symbols. got=${table.freeSymbols.length}, want=${expectedFreeSymbols.length}")

				expectedFreeSymbols.zipWithIndex.foreach { (symbol, index) =>
					if (table.freeSymbols(index) != symbol)
						fail(s"wrong free symbol. got=${table.freeSymbols(index)}, want=$symbol")
				}
		}
	}

	test("symbolTable.resolveUnresolvableFree") {
		val global = new SymbolTable()
		global.define("a")

		val local1 = new SymbolTable(outer = Some(global))
		local1.define("c")

		val local2 = new SymbolTable(outer = Some(local1))
		local2.define("e")
		local2.define("f")

		val expected = List(
			Symbol("a", GLOBAL, 0),
			Symbol("c", FREE, 0),
			Symbol("e", LOCAL, 0),
			Symbol("f", LOCAL, 1),
		)

		expected.foreach { symbol =>
			val result = local2.resolve(symbol.name)
			if (result != symbol)
				fail(s"expected ${symbol.name} to resolve to $symbol, got=$result")

		}

		val expectedUnresolvable = List("b", "d")

		expectedUnresolvable.foreach { name =>
			try {
				local2.resolve(name)
				fail(s"unexpectedly found symbol $name")
			}
			catch {
				case _: Exception =>
			}
		}
	}

	test("symbolTable.defineAndResolveFunctionName") {
		val global = new SymbolTable()
		global.defineFunctionName("a")

		val expected = Symbol("a", FUNCTION, 0)

		val result = global.resolve(expected.name)
		if (result != expected)
			fail(s"expected ${expected.name} to resolve to $expected, got=$result")
	}

	test("symbolTable.shadowingFunctionName") {
		val global = new SymbolTable()
		global.defineFunctionName("a")
		global.define("a")

		val expected = Symbol("a", GLOBAL, 0)

		val result = global.resolve(expected.name)
		if (result != expected)
			fail(s"expected ${expected.name} to resolve to $expected, got=$result")
	}

}
