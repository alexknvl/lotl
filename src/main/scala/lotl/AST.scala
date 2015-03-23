package lotl

import scala.annotation.tailrec

object AST {
  sealed trait Expr
  case class Cons(a: Expr, d: Expr) extends Expr
  case class Integer(value: BigInt) extends Expr
  case class Atom(symbol: Symbol) extends Expr
  val NilAtom: Expr = Atom('NIL)
  val TrueAtom: Expr = Atom('T)

  /**
   * A list of expressions such as (1 a (a b)) or ().
   */
  object ConsList {
    def apply(expressions: Expr*): Expr = expressions.foldRight(NilAtom: Expr)(Cons)
    def unapplySeq(expr: Expr): Option[Seq[Expr]] = expr match {
      case Cons(x, NilAtom) => Some(List(x))
      case Cons(head, tail: Cons) => unapplySeq(tail).map {head +: _}
      case NilAtom => Some(Nil)
      case _ => None
    }
  }

  /**
   * A list of symbols such as (a b c) or ().
   * Note the difference between ConsList and SymbolList is that
   * the elements of SymbolList are restricted to be symbols.
   */
  object SymbolList {
    def apply(atoms: Symbol*): Expr = ConsList(atoms.map(Atom): _*)
    def unapplySeq(expr: Expr): Option[Seq[Symbol]] = expr match {
      case Cons(Atom(x), NilAtom) => Some(List(x))
      case Cons(Atom(x), tail: Cons) => unapplySeq(tail).map {x +: _}
      case NilAtom => Some(Nil)
      case _ => None
    }
  }

  /**
   * A list of pairs such as ((a b) (1 c) (() ()))
   */
  object PairList {
    def apply(atoms: (Expr, Expr)*): Expr = ConsList(atoms.map { x => ConsList(x._1, x._2) }: _*)
    def unapplySeq(expr: AST.Expr): Option[Seq[(Expr, Expr)]] = expr match {
      case Cons(ConsList(x, y), NilAtom) => Some(List(x -> y))
      case Cons(ConsList(x, y), tail: Cons) => unapplySeq(tail).map {(x -> y) +: _}
      case NilAtom => Some(Nil)
      case _ => None
    }
  }

  object ConsMap {
    def apply(atoms: (Symbol, Expr)*): Expr = ConsList(atoms.map { x => Cons(Atom(x._1), x._2) }: _*)

    val empty: Expr = NilAtom

    @tailrec def get(map: Expr, name: Symbol): Option[Expr] = map match {
      case Cons(Cons(Atom(xn), xv), _) if xn == name => Some(xv)
      case Cons(_, xs) => get(xs, name)
      case NilAtom => None
    }

    def update(map: Expr, name: Symbol, value: Expr): Expr = map match {
      case Cons(Cons(Atom(n), v), tail) if n == name => Cons(Cons(Atom(n), value), tail)
      case Cons(x, xs) => Cons(x, update(xs, name, value))
      case NilAtom => Cons(Cons(Atom(name), value), NilAtom)
    }
  }
}
