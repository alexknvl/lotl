package lotl

trait StreamLike[S, E] {
  // Law: uncons(cons(e, s)) == Some((e, s))
  def cons(e: E, s: S): S
  def uncons(s: S): Option[(E, S)]

  def isEmpty(s: S): Boolean = uncons(s).isEmpty
  def nonEmpty(s: S): Boolean = uncons(s).nonEmpty

  def headOption(s: S): Option[E] = uncons(s).map { _._1 }
  def tailOption(s: S): Option[S] = uncons(s).map { _._2 }

  def toStream(s: S): Stream[E] = uncons(s) match {
    case Some((head, tail)) => Stream.cons(head, toStream(tail))
    case None => Stream.empty
  }
}

object StreamLike {
  implicit def streamInstance[E]: StreamLike[Stream[E], E] = new StreamLike[Stream[E], E] {
    override def cons(e: E, s: Stream[E]): Stream[E] = Stream.cons(e, s)
    override def uncons(s: Stream[E]): Option[(E, Stream[E])] =
      if (s.isEmpty) None else Some((s.head, s.tail))
  }
  implicit def listInstance[E]: StreamLike[List[E], E] = new StreamLike[List[E], E] {
    override def cons(e: E, s: List[E]): List[E] = e :: s
    override def uncons(s: List[E]): Option[(E, List[E])] =
      if (s.isEmpty) None else Some((s.head, s.tail))
  }
  implicit def seqInstance[E]: StreamLike[Seq[E], E] = new StreamLike[Seq[E], E] {
    override def cons(e: E, s: Seq[E]): Seq[E] = e +: s
    override def uncons(s: Seq[E]): Option[(E, Seq[E])] =
      if (s.isEmpty) None else Some((s.head, s.tail))
  }
  implicit object StringInstance extends StreamLike[String, Char] {
    override def cons(e: Char, s: String): String = e + s
    override def uncons(s: String): Option[(Char, String)] =
      if (s.isEmpty) None else Some((s.head, s.tail))
  }
}
