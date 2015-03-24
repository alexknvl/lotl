# "Lord of the LISP" Interpreter Project

## Disclaimer
I decided to use parser combinators instead of hand-writing LL(1) or recursive-descent parser because this seemed simpler and more fun. This of course means that I borrowed quite a few ideas from [1, 2, 3]. Furthermore, I borrowed function names and some ideas (e.g. using `case class ~[+A, +B]` for pattern matching) from Scala Standard Parser Combinator Library [4], and I used `MonadPlus`, `Show`, `ReaderT`, `StateT` and associated functions from Scalaz library [5].

That said, most of the code is extremely simple and concise.
* `SimpleRegexps.scala` is a very simple brzozowski-derivative based regular expression matcher (**Not used anywhere really, just for fun**).
* `Combinators.scala` provides parser combinators used in `Parser.scala`.
* `AST.scala` describes the internal representation used for S-expressions.
* `Parser.scala` describes a parser for S-expressions.
* `Printers.scala` describes several ways to print S-expressions.
* `Interpreter.scala` is the virtual machine for Lisp.
* `Main.scala` reads in a `$` separated file, parses it, evaluates and prints out the resulting S-expression.

## Grammar
I used the following grammar:
```
    // Primitives
    digit = [0-9]
    letter = [a-z] | [A-Z]
    space = [ \t\r\n]

    // Tokens
    id = letter (letter | digit)* space*
    int = ('+' | '-')? digit+ !letter space*
    open = '(' space*
    close = ')' space*
    dot = '.' space*
    nil = 'NIL' space* | 'nil' space*

    // Terms
    term = id | int | nil | dot | list
    dot = open term dot close
    list = open term* close
    statement = space* term EOF
```

## Internal Representation
I represent S-expressions using the following ADT:
```scala
    sealed trait Expr
    case class Cons(left: Expr, right: Expr) extends Expr
    case class Integer(value: BigInt) extends Expr
    case class Atom(symbol: Symbol) extends Expr
    val NilAtom: Expr = Atom('NIL)
    val TrueAtom: Expr = Atom('T)
```

## Pretty Printing
I implemented three different printing strategies:

* DotShow (default) which prints every cons cell as `(A . B)`.
    - `(1 2 3)` is printed as `(1 . (2 . (3 . NIL)))`
    - `(1 . 2)` is printed as `(1 . 2)`
* ListShow which prints everything that looks like a list using the list notation but otherwise falls back to using the dot notation.
    - `(1 2 3)` is printed as `(1 2 3)`
    - `(1 . 2)` is printed as `(1 . 2)`
* ConsShow assumes that `.` operator is right associative:
    - `(1 2 3)` is printed as `(1 . 2 . 3 . NIL)`
    - `(1 . 2)` is printed as `(1 . 2)`

## Interpreter
To be filled in...

## Appendix

### Parser Combinators
If you are interested in parser combinators, I suggest first reading [1, 2, 3], then [9, 10], then [6, 7, 8].

### Issues with Monadic Parser Combinators
Although monadic parser combinators provide us with an amazing way to write easily comprehensible grammars and parsers, they do have certain shortcomings:

* They do not handle left-recursive grammars well. However, there is a very interesting paper [6] that describes an approach based on Brzozowski's derivatives that makes it possible to use left-recursive grammars. Also see [7].
* Their composition operation (monadic bind) `Parser[E, A] => (A => Parser[E, B]) => Parser[E, B]` is a bit too powerful. Monadic parsers are capable of parsing context-sensitive grammars [8] and it is hard to statically analyze grammars [9]. However, it turns out that if we restrict parsers to Arrows instead of Monads this "problem" goes away [8, 9].
* It is not trivial to provide meaningful error messages [10].

## References
1. Graham Hutton and Erik Meijer. Monadic Parser Combinators. 1996.
2. Adriaan Moors, Frank Piessens and Martin Odersky. Parser combinators in Scala. 2008.
3. S. Doaitse Swierstra. Combinator parsing: A short tutorial. 2009.
4. https://github.com/scala/scala-parser-combinators
5. https://github.com/scalaz/scalaz
6. Matthew Might, David Darais. Yacc is dead. 2010.
7. Nils Anders Danielsson. Total parser combinators. 2010.
8. Daan Leijen, Erik Meijer. Parsec: Direct Style Monadic Parser Combinators
For The Real World. 2001
9. John Hughes. Generalizing monads to arrows. 2000.
10. S. Doaitse Swierstra. Parser combinators: from toys to tools. 2000.
