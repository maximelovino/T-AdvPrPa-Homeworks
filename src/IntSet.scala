abstract class IntSet() {
  def +(x: IntSet): IntSet

  def contains(x: Int): Boolean

  def foreach(f: Int => Unit): Unit

  def union(other: IntSet): IntSet

  def intersect(other: IntSet): IntSet

  def -(x: IntSet): IntSet

  def isEmpty: Boolean
}

object IntSet {
  implicit def int2IntSet(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
}
