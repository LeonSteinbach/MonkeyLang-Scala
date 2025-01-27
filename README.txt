Neben der Ausarbeitung meiner Bachelorarbeit ist eine ausführbare Version von Monkey mitgeliefert.
Für die Ausführung muss Java installiert sein.

java -jar Monkey.jar

Usage:  [options]

  --no-repl         Execute a predefined program instead of starting the REPL (used for testing)
  --parser <value>  Parser implementation (manual or combinator)
  --engine <value>  Engine implementation (interpreter or compiler)

-------------------------------------------------------------------------------------------------------------------

Beispiele für Monkey-Programme (die REPL unterstützt nur einzeilige Eingaben):

let fib = fn (n) { if (n < 2) { return n; }; fib(n-1) + fib(n-2); };
fib(10);

let fac = fn (n) { if (n < 1) { 1; } else { n * fac(n-1); };
fac(10);

let for = fn (from, to, body, accu) { if (from > to) { return accu; }; for (from+1, to, body, body(accu, from)); };
for(1, 5, fn (a, b) { a*b; }, 1);

-------------------------------------------------------------------------------------------------------------------

Die vollständige Implementierung von Monkey aus dieser Arbeit kann in meinem GitHub-Repository eingesehen werden:

https://github.com/LeonSteinbach/MonkeyLang-Scala
