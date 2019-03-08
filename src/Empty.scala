object Empty extends IntSet() {
  def contains(x: Int): Boolean = false

  def +(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  override def toString: String = "-"

  override def foreach(f: Int => Unit): Unit = Unit

  override def union(other: IntSet): IntSet = other

  override def intersect(other: IntSet): IntSet = this

  override def -(x: Int): IntSet = this

  override def isEmpty = true
}
