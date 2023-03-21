package de.hfu.monkey.Parser

import de.hfu.monkey.Ast.Program

trait Parser {
	def errors: Seq[String] = Seq()
	def parse(input: String): Program
}
