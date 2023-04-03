package de.hfu.monkey.compiler

import SymbolScope.*
import scala.collection.mutable

enum SymbolScope {
	case GLOBAL
}

case class Symbol(name: String, scope: SymbolScope, index: Int)

class SymbolTable(private val store: mutable.HashMap[String, Symbol] = mutable.HashMap.empty) {
	private var numDefinitions: Int = 0

	def define(name: String): Symbol = {
		val symbol = Symbol(name, GLOBAL, numDefinitions)
		store(name) = symbol
		numDefinitions += 1
		symbol
	}

	def resolve(name: String): Symbol = {
		store.getOrElse(name, throw new Exception(s"symbol $name not found"))
	}
}
