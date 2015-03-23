package lotl

import scala.{ specialized => sp }
import scala.collection.{ mutable, immutable }
import spire.math.Interval
import spire.algebra.Order
import spire.math.interval.Bound

object CharSets {
  final case class IntervalSet[@sp T: Order](intervals: List[Interval[T]])
}
