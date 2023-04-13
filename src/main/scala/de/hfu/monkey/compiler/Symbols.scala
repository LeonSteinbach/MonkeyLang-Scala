package de.hfu.monkey.compiler

import SymbolScope.*
import scala.collection.mutable

enum SymbolScope {
	case LOCAL, GLOBAL
}

case class Symbol(name: String, scope: SymbolScope, index: Int)

class SymbolTable(val outer: Option[SymbolTable] = None, private val store: mutable.HashMap[String, Symbol] = mutable.HashMap.empty) {
	var numDefinitions: Int = 0

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
					case Some(outer: SymbolTable) => outer.resolve(name)
					case None => throw new Exception(s"symbol not found: $name")
				}
		}
	}
}
