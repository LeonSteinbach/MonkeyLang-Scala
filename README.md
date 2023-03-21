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
                              | <block-statement>
<let-statement>             ::= "let" <identifier> "=" <expression> ";"
<return-statement>          ::= "return" <expression> ";"
<expression-statement>      ::= <expression> ";"
<block-statement>           ::= "{" <statement-list> "}"

<expression>                ::= <equality-expression>
<equality-expression>       ::= <comparative-expression> {("==" | "!=") <comparative-expression>}
<comparative-expression>    ::= <additive-expression> {("<" | ">") <additive-expression>}
<additive-expression>       ::= <multiplicative-expression> {("+" | "-") <multiplicative-expression>}
<multiplicative-expression> ::= <index-expression> {("*" | "/") <index-expression>}
<index-expression>          ::= <factor> {"[" <expression> "]"}
<factor>                    ::= <if-expression>
                              | <function-literal>
                              | <call-expression>
                              | <builtin-call-len>
                              | <builtin-call-arrays>
                              | <builtin-call-puts>
                              | <unary-expression>
                              | <value>
                              | "(" <expression> ")"

<unary-expression>          ::= ("-" | "!") <factor>
<call-expression>           ::= <identifier> "(" [<expression-list>] ")"
<builtin-call-len>          ::= "len" "(" (<string> | <array>) ")"
<builtin-call-arrays>       ::= ("first" | "last" | "tail") "(" <array> ")"
<builtin-call-puts>         ::= "puts" "(" [<expression-list>] ")"
<if-expression>             ::= "if" "(" <expression> ")" <block-statement> ["else" <block-statement>]
<function-literal>          ::= "fn" "(" [<parameter-list>] ")" <block-statement>

<value>                     ::= <identifier>
                              | <int>
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

<identifier>                ::= <alpha> { <alpha> | <digit> | "_" }
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
			<img src="https://github.com/LeonSteinbach/MonkeyLang-Scala/blob/main/benchmarks/parser/timings-parser-append.png" alt="Image 1" width="100%"/>
		</td>
		<td>
			<img src="https://github.com/LeonSteinbach/MonkeyLang-Scala/blob/main/benchmarks/parser/timings-parser-nested.png" alt="Image 2" width="100%"/>
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

