# MonkeyLang-Scala

This project implements an interpreter for the programming language Monkey, using Scala Parser Combinators. The project
is part of a bachelor's thesis on "Developing an Interpreter for the Programming Language Monkey using Scala Parser
Combinators".

# EBNF (Extended Backus-Naur form)

The EBNF below describes the grammar for MonkeyLang-Scala, as it is defined in the books.

It employs prioritized function calls to establish precedence for expressions, ensuring that the order of evaluation
respects the rules of arithmetic and logic.

In addition to the core language features, MonkeyLang-Scala also includes support for extended data structures from the
original Monkey programming language. These data structures comprise Strings, Arrays, and Hashes, which provide more
versatility and flexibility for users when programming in MonkeyLang-Scala.

```
<program>                   ::= <statement-list>
<statement-list>            ::= { <statement> }
<statement>                 ::= <let-statement>
                              | <return-statement>
                              | <expression-statement>
<let-statement>             ::= "let" <identifier> "=" <expression> ";"
<return-statement>          ::= "return" <expression> ";"
<expression-statement>      ::= <expression> ";"
<block-statement>           ::= "{" <statement-list> "}"

<expression>                ::= <equality-expression>
<equality-expression>       ::= <comparative-expression> {("==" | "!=") <comparative-expression>}
<comparative-expression>    ::= <additive-expression> {("<" | ">") <additive-expression>}
<additive-expression>       ::= <multiplicative-expression> {("+" | "-") <multiplicative-expression>}
<multiplicative-expression> ::= <prefix-expression> {("*" | "/") <prefix-expression>}
<prefix-expression>         ::= ("-" | "!") <prefix-expression>
                              | <postfix-expression>
<postfix-expression>        ::= <primary-expression> {<call-postfix> | <index-postfix>}
<call-postfix>              ::= "(" [<expression-list>] ")"
<index-postfix>             ::= "[" <expression> "]"
<primary-expression>        ::= <grouped-expression>
                              | <if-expression>
                              | <function>
                              | <identifier>
                              | <value>

<prefix-expression>         ::= ("-" | "!") <expression>
<grouped-expression>		::= "(" <expression> ")"
<if-expression>             ::= "if" "(" <expression> ")" <block-statement> ["else" <block-statement>]
<function>          		::= "fn" "(" [<parameter-list>] ")" <block-statement>
<identifier>                ::= <alpha> { <alpha> | <digit> | "_" }
<value>                     ::= <int>
                              | <bool>
                              | <string>
                              | <array>
                              | <hash>

<int>                       ::= <digit> { <digit> }
<digit>                     ::= "0..9"
<alpha>                     ::= "a..zA..Z"
<bool>                      ::= "true" | "false"
<string>                    ::= """ { <~any valid non-quotation-marks character> } """
<array>                     ::= "[" [<expression-list>] "]"
<hash>                      ::= "{" [<key-value-pairs>] "}"
<key-value-pairs>           ::= <expression> ":" <expression> { "," <expression> ":" <expression> }

<expression-list>           ::= <expression> { "," <expression> }
<parameter-list>            ::= <identifier> { "," <identifier> }
```

# Benchmarks

## Parser comparison

The benchmarks below present a comparison between the manual parser (blue) and the parser using Scala parser combinators (red).
The results demonstrate that as the text length increases linearly, the time required to parse the text also increases linearly.
Consequently, both parsers exhibit a complexity of O(n).

The manual parser is approximately 18 times faster than the parser employing Scala parser combinators.
The spike observed between a text length multiplier of 0 and 100 can be attributed to overhead and typical compilation issues.

It is noteworthy that the parser using Scala parser combinators experiences more significant fluctuations when dealing with nested function calls, as opposed to linearly increasing text. 

<table>
	<caption></caption>
	<tr>
		<td>
			<img src="https://github.com/LeonSteinbach/MonkeyLang-Scala/blob/main/benchmarks/parser/timings-parser-append.png" alt="timings-parser-append" width="100%"/>
		</td>
		<td>
			<img src="https://github.com/LeonSteinbach/MonkeyLang-Scala/blob/main/benchmarks/parser/timings-parser-nested.png" alt="timings-parser-nested" width="100%"/>
		</td>
	</tr>
	<tr>
		<td>
			Duration [ms] for parsing linear increasing text
		</td>
		<td>
			Duration [ms] for parsing increasingly nested function calls
		</td>
	</tr>
</table>

## Evaluator comparison

This benchmark presents a comparison between the tree-walking evaluator of MonkeyLang-Scala and other popular programming languages.
Specifically, the benchmark measures the duration in milliseconds it takes to compute the Fibonacci number with n=35 using a recursive implementation.

All benchmarks were performed on the same computer, an AMD Ryzen 5 5600x, to ensure consistency and fairness in the results.

<table>
	<caption></caption>
	<tr>
		<td>
			<img src="https://github.com/LeonSteinbach/MonkeyLang-Scala/blob/main/benchmarks/evaluator/timings-evaluator-fib.png" alt="timings-evaluator-fib" width="100%"/>
		</td>
	</tr>
	<tr>
		<td>
			Recursive fibonacci performance [n=35]
		</td>
	</tr>
</table>

