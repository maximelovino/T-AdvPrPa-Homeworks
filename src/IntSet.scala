abstract class IntSet() {
  def +(x: Int): IntSet

  def contains(x: Int): Boolean

  def foreach(f: Int => Unit): Unit

  def union(other: IntSet): IntSet

  def intersect(other: IntSet): IntSet

  def -(x: Int): IntSet

  def isEmpty: Boolean
}
