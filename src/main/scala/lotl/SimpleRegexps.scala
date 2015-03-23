package lotl

import scala.language.higherKinds
import scala.collection.{mutable, immutable}

object SimpleRegexps {
  sealed trait Regex[E]
  final case class Literal[E](e: E) extends Regex[E]
  final case class Product[E](args: List[Regex[E]]) extends Regex[E]
  final case class Sum[E](args: List[Regex[E]]) extends Regex[E]
  final case class And[E](args: List[Regex[E]]) extends Regex[E]
  final case class Not[E](arg: Regex[E]) extends Regex[E]
  final case class Star[E](arg: Regex[E]) extends Regex[E]

  // Zeros
  // ∧{} = ∀
  // ∨{} = ∅
  // ∘{} = ε
  object Empty {
    def apply[E](): Regex[E] = Sum[E](Nil)
    def unapply[E](re: Regex[E]): Boolean = re match {
      case Sum(Nil) => true
      case _ => false
    }
  }
  object Epsilon {
    def apply[E](): Regex[E] = Product[E](Nil)
    def unapply[E](re: Regex[E]): Boolean = re match {
      case Product(Nil) => true
      case _ => false
    }
  }
  object All {
    def apply[E](): Regex[E] = And[E](Nil)
    def unapply[E](re: Regex[E]): Boolean = re match {
      case And(Nil) => true
      case _ => false
    }
  }

  // Ones
  // ∧{x} = x
  // ∨{x} = x
  // ∘{x} = x
  final def simplifyOnes[E](re: Regex[E]): Regex[E] = re match {
    case And(x :: Nil) => x
    case Sum(x :: Nil) => x
    case Product(x :: Nil) => x
    case x => x
  }

  // Identities and Annihilators
  // ∀ | r = ∀, ∅ | r = r
  // ∅ ∧ r = ∅, ∀ ∧ r = r
  // ∅ ∘ r = ∅, r ∘ ∅ = ∅
  // ε ∘ r = r, r ∘ ε = r
  final def simplifyByIdentityLaws[E](re: Regex[E]): Regex[E] = re match {
    case Sum(xs) =>
      val ys = xs.filterNot(Empty.unapply)
      if (ys.exists(All.unapply)) All()
      else Sum(ys)
    case And(xs) =>
      val ys = xs.filterNot(All.unapply)
      if (ys.exists(Empty.unapply)) Empty()
      else And(ys)
    case Product(xs) =>
      val ys = xs.filterNot(Epsilon.unapply)
      if (ys.exists(Empty.unapply)) Empty()
      else Product(ys)
    case x => x
  }

  // Negation
  // ¬¬x = x
  // ¬∅ = ∀
  // ¬∀ = ∅
  // ¬ε = Σ∘∀
  final def simplifyByNegationLaws[E](re: Regex[E]): Regex[E] = re match {
    case Not(Not(x)) => x
    case Not(Empty()) => All()
    case Not(All()) => Empty()
    case x => x
  }

  // Kleene star
  // x** = x*
  // ε* = ε
  // ∅* = ε
  // Σ* = ∀
  // ∀* = ∀
  final def simplifyByStarLaws[E](re: Regex[E]): Regex[E] = re match {
    case Star(Star(x)) => Star(x)
    case Star(Epsilon()) => Epsilon()
    case Star(Empty()) => Epsilon()
    case Star(All()) => All()
    case x => x
  }

  // Associativity
  // (r ∘ s) ∘ t = r ∘ (s ∘ t)
  // (r ∧ s) ∧ t = r ∧ (s ∧ t)
  // (r ∨ s) ∨ t = r ∨ (s ∨ t)

  // Commutativity
  // r ∨ s = s ∨ r
  // r ∧ s = s ∧ r
  def flatten[E](list: List[Regex[E]], unapply: PartialFunction[Regex[E], List[Regex[E]]]): List[Regex[E]] = {
    val buffer = mutable.ListBuffer(list :_*)
    var index = 0

    while (index < buffer.size) {
      val re = buffer(index)
      //println(s"flatten ! $index ${unapply.lift(re)}")
      unapply.lift(re) match {
        case Some(Nil) => buffer.remove(index)
        case Some(xs) =>
          buffer.remove(index)
          buffer.insertAll(index, xs)
        case None => index += 1
      }
    }

    buffer.toList
  }
  def simplifyByAssociativityAndCommutativity[E](re: Regex[E]): Regex[E] = re match {
    case Sum(xs) => Sum(flatten(xs, { case Sum(ys) => ys }))
    case And(xs) => And(flatten(xs, { case And(ys) => ys }))
    case Product(xs) => Product(flatten(xs, { case Product(ys) => ys }))
    case x => x
  }

  // Idempotent laws
  // x ∧ x = x
  // x ∨ x = x

  // Distributivity
  // x ∘ (y ∨ z) = (x ∘ y) ∨ (x ∘ z)
  // x ∘ (y ∧ z) = (x ∘ y) ∧ (x ∘ z)
  // (x ∧ y) ∘ z = (x ∘ z) ∧ (y ∘ z)
  // ...?

  // De Morgan laws
  // ¬(∧Xi) = ∨(¬Xi)

  def fullSimplify[E](self: Regex[E]): Regex[E] = {
    var value: Regex[E] = self
    var done: Boolean = false

    while (!done) {
      var newValue = value

      // println(s"recursive $newValue")
      // Recursive simplification
      newValue = newValue match {
        case l@Literal(e) => l
        case Product(xs) => Product(xs.map(fullSimplify))
        case Sum(xs) => Sum(xs.map(fullSimplify))
        case And(xs) => And(xs.map(fullSimplify))
        case Star(x) => Star(fullSimplify(x))
        case Not(x) => Not(fullSimplify(x))
      }

      // println(s"simple $newValue")
      newValue = simplifyByAssociativityAndCommutativity(newValue)
      // println(s"simplifyByAssociativityAndCommutativity -> $newValue")
      newValue = simplifyByIdentityLaws(newValue)
      // println(s"simplifyByIdentityLaws -> $newValue")
      newValue = simplifyByNegationLaws(newValue)
      // println(s"simplifyByNegationLaws -> $newValue")
      newValue = simplifyByStarLaws(newValue)
      // println(s"simplifyByStarLaws -> $newValue")
      newValue = simplifyOnes(newValue)
      // println(s"simplifyOnes -> $newValue")

      // Fixed point.
      done = newValue == value
      // println(s"$value -> $newValue ($done)")
      value = newValue
    }

    value
  }

  def nullable[E](re: Regex[E]): Boolean = re match {
    case l@Literal(_) => false
    case Product(args) => args.forall(nullable)
    case And(args) => args.forall(nullable)
    case Sum(args) => args.exists(nullable)
    case Not(arg) => !nullable(arg)
    case Star(_) => true
  }
  def delta[E](self: Regex[E]): Regex[E] = if (nullable(self)) Epsilon() else Empty()

  def brzozowskiLeft[E](e: E, self: Regex[E])(implicit eq: Equiv[E]): Regex[E] = {
    type R = Regex[E]
    type LR = List[R]

    self match {
      case l@Literal(v) if eq.equiv(e, v) => Epsilon()
      case l@Literal(_) => Empty()

      case Empty() => Empty()
      case Epsilon() => Empty()
      case All() => All()

      case Sum(xs) => Sum(xs.map(brzozowskiLeft(e, _)))
      case And(xs) => And(xs.map(brzozowskiLeft(e, _)))
      case Product(a) =>
        // e /: (x ~ y ~ z) = e /: z | e /: y ~ z | e /: x ~ y ~ z
        // provided that x and y are nullable
        def f(list: List[R]): List[R] = brzozowskiLeft(e, list.head) :: list.tail

        val (xs: List[R], ys: List[R]) = a.span(nullable)
        val zs: List[List[R]] = xs.scanRight(ys){ (x: R, ys: LR) => x :: ys }
        val groups: List[List[R]] = zs.map { f }
        groups match {
          case product :: Nil => Product(product)
          case products => Sum(products.map(Product[E]))
        }

      case Not(x) => Not(brzozowskiLeft(e, x))
      case Star(x) => Product(List(brzozowskiLeft(e, x), Star(x)))
    }
  }

  implicit class OpWrapper[E](regex: Regex[E]) {
    // Precedence: | & / ~
    def ~(other: Regex[E]): Regex[E] = Product(List(regex, other))
    def |(other: Regex[E]): Regex[E]= Sum(List(regex, other))
    def * : Regex[E] = Star(regex)
    def + : Regex[E] = Product(List(regex, Star(regex)))
    def ? : Regex[E] = Sum(List(regex, Epsilon()))
    def /:(e: E)(implicit eq: Equiv[E]): Regex[E] = fullSimplify(brzozowskiLeft(e, regex))

    def matchLeft(es: E*)(implicit eq: Equiv[E]): Regex[E] =
      es.foldLeft(regex)((re, e) => fullSimplify(brzozowskiLeft(e, re)))
  }

  implicit def char2Regex(ch: Char): Regex[Char] = Literal(ch)
  implicit def string2Regex(str: String): Regex[Char] = Product(str.map(Literal(_)).toList)

  implicit def char2Wrapper(ch: Char): OpWrapper[Char] =
    new OpWrapper[Char](char2Regex(ch))
  implicit def string2Wrapper(str: String): OpWrapper[Char] =
    new OpWrapper[Char](string2Regex(str))
}
