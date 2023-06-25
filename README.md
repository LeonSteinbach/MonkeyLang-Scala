# MonkeyLang-Scala

This project implements an interpreter for the programming language Monkey, using Scala Parser Combinators. The project
is part of a bachelor's thesis on "Developing an Interpreter for the Programming Language Monkey using Scala Parser
Combinators".

# EBNF (Extended Backus-Naur form)

The EBNF below describes the grammar for Monkey, as it is defined in the books "Writing an Interpreter in Go" and "Writing a Compiler in Go" from Thorsten Ball.

It employs operator precedence for expressions, ensuring that the order of evaluation
respects the rules of arithmetic and logic.

In addition to the core language features, Monkey also includes support for extended data structures from the
original Monkey programming language. These data structures are Strings, Arrays, and Hashes, which provide more
versatility and flexibility for users when programming in Monkey.

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
