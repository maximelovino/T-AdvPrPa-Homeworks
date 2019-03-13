

class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  def +(x1: IntSet): IntSet = {
    val x = x1.asInstanceOf[NonEmpty].elem
    if (x < elem) {
      new NonEmpty(elem, left + x1, right)
    } else if (x > elem) {
      new NonEmpty(elem, left, right + x1)
    } else {
      this
    }
  }

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x else if (x > elem) right contains x else true
  }

  override def toString: String = s"($left|$elem|$right)"

  override def foreach(f: Int => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def union(other: IntSet): IntSet = {
    left union right union other + elem
  }

  override def intersect(other: IntSet): IntSet = {
    if (other contains elem) {
      new NonEmpty(elem, left intersect other, right intersect other)
    } else {
      left union right intersect other
    }
  }

  override def -(x1: IntSet): IntSet = {
    val x = x1.asInstanceOf[NonEmpty].elem
    if (x < elem) {
      new NonEmpty(elem, left - x1, right)
    } else if (x > elem) {
      new NonEmpty(elem, left, right - x1)
    } else {
      left.union(right)
    }
  }

  override def isEmpty = false
}

