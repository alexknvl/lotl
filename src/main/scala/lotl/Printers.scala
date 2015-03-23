package lotl

import AST._

object Printers {
  implicit object DotShow extends scalaz.Show[Expr] {
    override def shows(expr: Expr): String = expr match {
      case NilAtom => "NIL"
      case Integer(value) => s"$value"
      case Atom(Symbol(symbol)) => s"$symbol";
      case Cons(left, right) =>
        s"(${shows(left)} . ${shows(right)})"
    }
  }

  implicit object ListShow extends scalaz.Show[Expr] {
    override def shows(expr: Expr): String = expr match {
      case NilAtom => "NIL"
      case Integer(value) => s"$value"
      case Atom(Symbol(symbol)) => s"$symbol";
      case ConsList(list @_*) =>
        val strList = list.map { shows }
        s"(${strList mkString " "})"
      case Cons(left, right) =>
        s"(${shows(left)} . ${shows(right)})"
    }
  }

  implicit object ConsShow extends scalaz.Show[Expr] {
    override def shows(expr: Expr): String = expr match {
      case NilAtom => "NIL"
      case Integer(value) => s"$value"
      case Atom(Symbol(symbol)) => s"$symbol"
      case ConsList(list @_*) =>
        val strList = list.map { shows } ++ List("NIL")
        s"(${strList mkString " . "})"
      case Cons(left, right) =>
        s"(${shows(left)} . ${shows(right)})"
    }
  }
}
