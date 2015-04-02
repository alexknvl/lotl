# "Lord of the LISP" Interpreter Project

## Disclaimer
This project is available on my [GitHub page](https://github.com/alexknvl/lotl). I did it as a part of my course on programming languages.

I decided to use parser combinators instead of hand-writing LL(1) or recursive-descent parser because this seemed simpler and a lot more fun. This of course means that I borrowed quite a few ideas from [1, 2, 3]. Furthermore, I borrowed function names and some ideas (e.g. using `case class ~[+A, +B]` for pattern matching) from Scala Standard Parser Combinator Library [4], and I used `MonadPlus`, `Show`, `ReaderT`, `StateT` and associated functions from Scalaz library [5].

That said, most of the code is extremely simple and concise.
* `SimpleRegexps.scala` is a simple Brzozowski derivative [11] based regular expression matcher. I pretty much followed [12] while implementing it. I plan to add other derivatives using McBride's dissection [13] and implement group capture / reductions later on. **Not used anywhere really, just for fun**.
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

### Types
Most special forms and functions may fail one way or another, so we need some way to represent possible failure. I encode this using a disjunction:
```scala
    type Result[+A] = \/[String, A]
```

Next, some of the forms in Lisp may modify global state (e.g. `DEFUN`) and most read it: 
```scala
    // This type encodes something that takes an environment Env 
    // and returns some value. `ReaderT` (reader monad transformer) 
    // is a very thin wrapper around `Env => Result[A]` provided by scalaz.
    // For most purposes this type is equivalent to `Env => Result[A]`.
    type PureEval[A] = ReaderT[Result, Env, A]

    // This type encodes something that takes an environment Env 
    // and returns some value together with a new environment. 
    // `StateT` (reader monad transformer) is a very thin wrapper 
    // around `Env => Result[(Env, A)]` provided by scalaz.
    // For most purposes this type is equivalent to 
    // `Env => Result[(Env, A)]`.
    type Eval[A] = StateT[Result, Env, A]

    // The environment. Contains all visible local variables, 
    // user-defined and predefined functions, 
    // pure and state-modyfing forms.
    case class Env(locals: Map[Symbol, Expr], functions: Map[Symbol, PureForm],
                   pureForms: Map[Symbol, PureForm], forms: Map[Symbol, Form])
```

Next, we add input parameters. I used partial functions to represent the fact that forms and functions only accept a certain number of arguments of certain types (and I regret this decision, there should be some better way).
```scala
    type =>?[-A, +B] = PartialFunction[A, B]
    // For most purposes this type is equivalent to 
    // `Expr =>? Env => Result[Expr]`
    type PureForm = Expr =>? PureEval[Expr]
    // `Expr =>? Env => Result[(Env, Expr)]`
    type Form = Expr =>? Eval[Expr]
```

I decided to use the `PureForm` type for functions such as `CAR` and `PLUS` or user-defined function as well as for pure forms like `QUOTE`. Since many functions have a simpler type `Expr =>? Expr`, I defined a conversion operation:
```scala
    def lift(f: Expr =>? Expr): PureForm = new =>?[Expr, PureEval[Expr]] {
      override def isDefinedAt(x: Expr): Boolean = f.isDefinedAt(x)
      override def apply(v: Expr): PureEval[Expr] = PureEval { e => f(v).right}
    }
```

### Basic Forms
I implemented the following forms:
```scala
    // (QUOTE x)
    val quote: PureForm
    // (COND (A B) (T C))
    val cond: PureForm
    // (DEFUN F (a b) (PLUS a b))
    val defun: Form
    // Evaluates the given expression within the given environment.
    // *Is not available from Lisp.* 
    val eval: PureForm
    // Evaluates the given expression within the given environment.
    // Supports state-modyfing forms.
    // *Is not available from Lisp.* 
    val topEval: Form
    // Resets the environment to the initial (clean) state.
    def clear: Form
    // Dumps the environment as a Lisp-expression.
    // Can be used for debugging.
    def dumpenv: PureForm
```

## Appendix

### Parser Combinators
If you are interested in parser combinators, I suggest first reading [1, 2, 3], then [9, 10], then [6, 7, 8].

### Issues with Monadic Parser Combinators
Although monadic parser combinators provide us with an amazing way to write easily comprehensible grammars and parsers, they do have certain shortcomings:

* They do not handle left-recursive grammars well. However, there is a very interesting paper [6] that describes an approach based on Brzozowski's derivatives that makes it possible to use left-recursive grammars. Also see [7].
* Their composition operation (monadic bind) `Parser[E, A] => (A => Parser[E, B]) => Parser[E, B]` is a bit too powerful. Monadic parsers are capable of parsing context-sensitive grammars [8] and the resulting parsers are hard to analyze during compile-time [9]. However, it turns out that if we restrict parsers to Arrows instead of Monads this "problem" goes away [8, 9].
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
11. http://en.wikipedia.org/wiki/Brzozowski_derivative
12. http://www.mpi-sws.org/~turon/re-deriv.pdf
13. http://strictlypositive.org/Dissect.pdf
