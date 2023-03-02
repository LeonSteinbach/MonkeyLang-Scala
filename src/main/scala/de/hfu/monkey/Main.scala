package de.hfu.monkey

object Main extends App {

  private val parser = new Parser()
  private val result = parser.parseAll(parser.program, "a(1) - b(2);")
  println(result)

}
