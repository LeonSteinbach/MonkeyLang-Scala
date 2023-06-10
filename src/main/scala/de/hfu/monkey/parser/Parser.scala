package de.hfu.monkey.parser

import de.hfu.monkey.ast.Program

trait Parser {
	def errors: Seq[String] = Seq()
	def parse(input: String): Program
}

class ParserException(message: String) extends RuntimeException(message)
