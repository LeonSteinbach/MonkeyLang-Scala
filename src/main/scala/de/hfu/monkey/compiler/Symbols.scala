package de.hfu.monkey.compiler

import SymbolScope.*
import scala.collection.mutable

enum SymbolScope {
	case LOCAL, GLOBAL, FREE, BUILTIN, FUNCTION
}

case class Symbol(name: String, var scope: SymbolScope, index: Int)

class SymbolTable(val outer: Option[SymbolTable] = None, private val store: mutable.HashMap[String, Symbol] = mutable.HashMap.empty) {
	var numDefinitions: Int = 0
	var freeSymbols: List[Symbol] = List.empty[Symbol]

	def define(name: String): Symbol = {
		val scope = if (outer.isDefined) LOCAL else GLOBAL
		val symbol = Symbol(name, scope, numDefinitions)
		store(name) = symbol
		numDefinitions += 1
		symbol
	}

	def resolve(name: String): Symbol = {
		store.get(name) match {
			case Some(symbol: Symbol) => symbol
			case None =>
				outer match {
					case Some(outer: SymbolTable) =>
						val obj = outer.resolve(name)
						if (obj.scope == GLOBAL || obj.scope == BUILTIN)
							obj
						else
							defineFree(obj)
					case None => throw new Exception(s"symbol not found: $name")
				}
		}
	}

	private def defineFree(original: Symbol): Symbol = {
		freeSymbols = freeSymbols :+ original
		val symbol = Symbol(original.name, FREE, freeSymbols.length - 1)
		symbol.scope = FREE
		store(original.name) = symbol
		symbol
	}

	def defineFunctionName(name: String): Symbol = {
		val symbol = Symbol(name, FUNCTION, 0)
		store(name) = symbol
		symbol
	}
}
