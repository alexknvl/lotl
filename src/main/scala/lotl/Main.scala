package lotl

import lotl.Printers._
import scala.io.Source
import scala.util.control.Breaks._
import scalaz._

trait IteratorExtra {
  implicit class IteratorExtra[A](val iterator: Iterator[A]) {
    def splitBy(p: A => Boolean): Iterator[IndexedSeq[A]] = new Iterator[IndexedSeq[A]] {
      private val current: Iterator[A] = iterator
      override def hasNext: Boolean = current.hasNext

      override def next(): IndexedSeq[A] = {
        import scala.collection.mutable.ArrayBuffer
        val result = ArrayBuffer.empty[A]
        var done = false

        while (!done && current.hasNext) {
          val v = current.next()
          if (p(v)) {
            done = true
          } else {
            result += v
          }
        }

        result
      }
    }
  }
}

object Main extends IteratorExtra {
  def main(args: Array[String]): Unit = {
    val input =
      if (args.length == 1) Source.fromFile(args(0))
      else Source.fromInputStream(System.in)

    var state = Interpreter.prelude

    breakable {
      for (str <- input.splitBy(_ == '$').map { _.mkString }) {
        if (str.isEmpty) break()

        val input: Stream[Char] = str.toStream
        val results = Parser.statement(input)

        results.headOption match {
            case None => println("** parsing error **")
            case Some((expr, _)) => Interpreter.topEval(expr)(state) match {
              case -\/(message: String) => println(message)
              case \/-((newState, result)) =>
                println(ListShow.shows(result))
                state = newState
            }
        }
      }
    }
  }
}
