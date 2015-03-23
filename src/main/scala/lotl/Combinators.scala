package lotl

import scala.language.{postfixOps, higherKinds, implicitConversions}
import scalaz._
import Scalaz._

object Combinators {
  type Input[+E] = Stream[E]
  type ParserMonad[A] = Option[A]
  type Result[+E, +A] = ParserMonad[(A, Input[E])]
  val ResultMonadPlus = implicitly[MonadPlus[ParserMonad]]

  abstract class Parser[E, +A] extends (Input[E] => Result[E, A]) {
    def apply(input: Input[E]): Result[E, A]
  }

  object Parser {
    def apply[E, A](f: Input[E] => Result[E, A]): Parser[E, A] = new Parser[E, A] {
      def apply(input: Input[E]): Result[E, A] = f(input)
    }
  }

  trait Parsers {
    final class Sum[E, A](lhs: => Parser[E, A], rhs: => Parser[E, A]) extends Parser[E, A] {
      def apply(input: Input[E]): Result[E, A] =
        ResultMonadPlus.plus(lhs(input), rhs(input))
    }

    case class ~[+A, +B](_1: A, _2: B)
    final class Product[E, A, B](lhs: => Parser[E, A], rhs: => Parser[E, B]) extends Parser[E, ~[A, B]] {
      def apply(s0: Input[E]): Result[E, ~[A, B]] =
        for {
          (a, s1) <- lhs(s0)
          (b, s2) <- rhs(s1)
        } yield (new ~(a, b), s2)
    }

    final class Map[E, +A, +B](parser: => Parser[E, A], f: A => B) extends Parser[E, B] {
      def apply(s0: Input[E]): Result[E, B] =
        for {
          (v, s1) <- parser(s0)
        } yield (f(v), s1)
    }

    def success[E, A](a: A): Parser[E, A] = Parser { input: Input[E] =>
      ResultMonadPlus.point((a, input))
    }

    implicit def elem(char: Char): Parser[Char, Char] = Parser { input: Input[Char] =>
      if (input.isEmpty) ResultMonadPlus.empty
      else if (input.head == char) ResultMonadPlus.point((input.head, input.tail))
      else ResultMonadPlus.empty
    }

    implicit def string(string: String): Parser[Char, String] = Parser { input: Input[Char] =>
      var rest: Input[Char] = input
      var result: Option[Boolean] = None
      var offset: Int = 0

      while (result.isEmpty) {
        if (offset >= string.length) result = Some(true)
        else {
          if (rest.nonEmpty && rest.head == string.charAt(offset)) {
            offset += 1
            rest = rest.tail
          } else result = Some(false)
        }
      }

      if (result.get) ResultMonadPlus.point((string, rest))
      else ResultMonadPlus.empty
    }

    implicit def range[E](range: scala.collection.immutable.NumericRange[E]): Parser[E, E] = Parser { input: Input[E] =>
      if (input.isEmpty || !range.contains(input.head)) ResultMonadPlus.empty
      else ResultMonadPlus.point((input.head, input.tail))
    }

    def oneOf[E, Repr](elems: scala.collection.SeqLike[E, Repr]): Parser[E, E] = Parser { input: Input[E] =>
      if (input.isEmpty || !elems.contains(input.head)) ResultMonadPlus.empty
      else ResultMonadPlus.point((input.head, input.tail))
    }

    def not[E, A](parser: => Parser[E, A]): Parser[E, Unit] = Parser { input: Input[E] =>
      val result = parser(input)
      if (result.isEmpty) ResultMonadPlus.point(((), input))
      else ResultMonadPlus.empty
    }

    def opt[E, A](parser: => Parser[E, A]): Parser[E, Option[A]] =
      parser ^^ {x => Some(x):Option[A]} | success(None)

    private def mkList[A](pair: ~[A, List[A]]) = pair match { case a ~ b => a :: b }
    def rep[E, A](parser: => Parser[E, A]) : Parser[E, List[A]] =
      new Sum(rep1(parser), success(List()))
    def rep1[E, A](parser: => Parser[E, A]) : Parser[E, List[A]] =
      rep1(parser, parser)
    def rep1[E, A](first: => Parser[E, A], rest: => Parser[E, A]) : Parser[E, List[A]] =
      new Map(new Product(first, rep(rest)), mkList[A])

    def eof[E]: Parser[E, Unit] = Parser { input: Input[E] =>
      if (input.isEmpty) ResultMonadPlus.point(((), input))
      else ResultMonadPlus.empty
    }

    implicit class OpWrapper[E, A](parser: => Parser[E, A]) {
      def |(other: => Parser[E, A]): Parser[E, A] = new Sum(parser, other)
      def ^^[B](f: A => B): Parser[E, B] = new Map(parser, f)
      def ^^^[B](f: => B): Parser[E, B] = new Map(parser, {x: A => f})

      def ~[B](other: => Parser[E, B]): Parser[E, ~[A, B]] = new Product(parser, other)
      def ~>[B](other: => Parser[E, B]): Parser[E, B] = parser ~ other ^^ { case a ~ b => b }
      def <~[B](other: => Parser[E, B]): Parser[E, A] = parser ~ other ^^ { case a ~ b => a }

      def unary_! : Parser[E, Unit] = not(parser)
      def ? : Parser[E, Option[A]] = opt(parser)
      def + : Parser[E, List[A]] = rep1(parser)
      def * : Parser[E, List[A]] = rep(parser)

      def >>:[B](other: Parser[E, A => B]): Parser[E, B] = Parser { input: Input[E] =>
        for {
          (f, s1) <- other(input)
          (v, s2) <- parser(s1)
        } yield (f(v), s2)
      }
      def >>>[B](other: Parser[E, A => B]): Parser[E, B] = Parser { input: Input[E] =>
        for {
          (v, s1) <- parser(input)
          (f, s2) <- other(s1)
        } yield (f(v), s2)
      }
    }
  }
}
