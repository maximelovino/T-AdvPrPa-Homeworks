class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {
  def +(x: Int): IntSet = {
    if (x < elem) {
      new NonEmpty(elem, left + x, right)
    } else if (x > elem) {
      new NonEmpty(elem, left, right + x)
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
    def rec(currentTree: IntSet, treeToAdd: IntSet): IntSet = {
      if (treeToAdd.isEmpty)
        currentTree
      else {
        val nonEmptyTree = treeToAdd.asInstanceOf[NonEmpty]
        rec(currentTree.+(nonEmptyTree.elem), nonEmptyTree.left.union(nonEmptyTree.right))
      }
    }

    rec(this, other)
  }

  override def intersect(other: IntSet): IntSet = {
    def rec(currentTree: IntSet, a: IntSet, b: IntSet): IntSet = {
      if (a.isEmpty || b.isEmpty) {
        currentTree
      } else {
        val nonEmptyA = a.asInstanceOf[NonEmpty]
        val nonEmptyB = b.asInstanceOf[NonEmpty]
        rec(
          if (nonEmptyA.contains(nonEmptyB.elem)) {
            currentTree.+(nonEmptyB.elem)
          } else {
            currentTree
          },
          nonEmptyA,
          nonEmptyB.left.union(nonEmptyB.right)
        )
      }
    }

    rec(Empty, this, other)
  }

  override def -(x: Int): IntSet = {
    if (x < elem) {
      new NonEmpty(elem, left - x, right)
    } else if (x > elem) {
      new NonEmpty(elem, left, right - x)
    } else {
      //TODO is it really useful or should union take care of this
      if (left.isEmpty) {
        right
      } else if (right.isEmpty) {
        left
      } else {
        left.union(right)
      }
    }
  }

  override def isEmpty = false
}