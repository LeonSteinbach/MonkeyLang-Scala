package de.hfu.monkey

object Main extends App {

  private val parser = new Parser()
  private val result = parser.parseAll(parser.program,
    "fn (a, b) { a + b; };")
  println(result)

}
