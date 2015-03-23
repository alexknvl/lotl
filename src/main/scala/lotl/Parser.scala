package lotl

import Combinators._

object Parser extends Parsers {
  object primitive {
    def digit: Parser[Char, Char] = range('0' to '9')
    def letter: Parser[Char, Char] = range('a' to 'z') | range('A' to 'Z')
    def space: Parser[Char, Char] = oneOf(" \t\n\r")
  }

  object tokens {
    import primitive._

    def id0: Parser[Char, String] = rep1(letter, letter | digit) ^^ { _.mkString }
    def int0: Parser[Char, String] = opt(oneOf("+-")) ~ rep1(digit) <~ !letter ^^ {
      case Some(a) ~ b => a + b.mkString
      case None ~ b => b.mkString
    }

    def token[A](parser: Parser[Char, A]): Parser[Char, A] = parser <~ space.*

    def open: Parser[Char, String] = token("(")
    def close: Parser[Char, String] = token(")")
    def dot: Parser[Char, String] = token(".")
    def id: Parser[Char, String] = token(id0)
    def int: Parser[Char, String] = token(int0)
    def nil: Parser[Char, String] = token("NIL") | token("nil")
  }

  def nil: Parser[Char, AST.Expr] = tokens.nil ^^^ AST.NilAtom
  def id: Parser[Char, AST.Expr] = tokens.id ^^ { x: String => AST.Atom(Symbol(x)) }
  def int: Parser[Char, AST.Expr] = tokens.int ^^ { x: String => AST.Integer(BigInt(x)) }
  def term: Parser[Char, AST.Expr] = nil | id | int | dot | list
  def dot: Parser[Char, AST.Expr] = tokens.open ~> (term <~ tokens.dot) ~ term <~ tokens.close ^^ { s => AST.Cons(s._1, s._2) }
  def list: Parser[Char, AST.Expr] = tokens.open ~> rep(term) <~ tokens.close ^^ ( AST.ConsList(_:_*) )
  def statement: Parser[Char, AST.Expr] = primitive.space.* ~> term <~ eof
}
