package de.hfu.monkey

object Main extends App {

  private val startTime = System.currentTimeMillis()
  private val parser = new Parser()
  private val parsed = parser.parseAll(parser.program,
    "let fib = fn(n) { if (n < 2) { return n; }; fib(n - 1) + fib(n - 2); }; fib(35);")
  private val evaluated = Evaluator.evaluateProgram(parsed.get, new Environment)
  private val endTime = System.currentTimeMillis()

  println(parsed)
  println(evaluated)
  println(s"Duration [ms]: ${endTime - startTime}")

}
