package de.hfu.monkey

object Main extends App {

  private val parser = new Parser()
  private val result = parser.parseAll(parser.program,
    "let fib = fn(n) { " +
    "  if (n < 2) {" +
    "    n; " +
    "  };" +
    "  fib(n-1) + fib(n-2);" +
    "};" +
    "fib(10);")
  println(result)

}
