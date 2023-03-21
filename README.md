# MonkeyLang-Scala

This project implements an interpreter for the programming language Monkey, using Scala Parser Combinators. The project is part of a bachelor's thesis on "Developing an Interpreter for the Programming Language Monkey using Scala Parser Combinators".

# EBNF (Extended Backus-Naur form)

The EBNF below describes the grammar for MonkeyLang-Scala, as it is defined in the books.

It employs prioritized function calls to establish precedence for expressions, ensuring that the order of evaluation respects the rules of arithmetic and logic.

In addition to the core language features, MonkeyLang-Scala also includes support for extended data structures from the original Monkey programming language. These data structures comprise Strings, Arrays, and Hashes, which provide more versatility and flexibility for users when programming in MonkeyLang-Scala.

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
<string>                    ::= """ { <any-character> } """
<array>                     ::= "[" [<expression-list>] "]"
<hash>                      ::= "{" [<key-value-pairs>] "}"
<key-value-pairs>           ::= <expression> ":" <expression> { "," <expression> ":" <expression> }

<identifier>                ::= <alpha> { <alpha> | <digit> | "_" }
<expression-list>           ::= <expression> { "," <expression> }
<parameter-list>            ::= <identifier> { "," <identifier> }
```