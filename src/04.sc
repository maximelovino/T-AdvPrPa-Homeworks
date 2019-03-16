//Question 1

sealed abstract class Expr

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Product(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Product(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + "+" + show(e2)
  case Product(e1, e2) => (e1, e2) match {
    case (Sum(_, _), Sum(_, _)) => s"(${show(e1)})*(${show(e2)})"
    case (Sum(_, _), _) => s"(${show(e1)})*${show(e2)}"
    case (_, Sum(_, _)) => s"${show(e1)}*(${show(e2)})"
    case _ => s"${show(e1)}*${show(e2)}"
  }
}

val expr0 = Sum(Product(Number(2), Number(3)), Number(4))
println("Expr0: " + show(expr0))
assert(eval(expr0) == 10)

val expr1 = Product(Number(4), Number(12))
println("Expr1: " + show(expr1))
assert(eval(expr1) == 48)

val expr2 = Product(Sum(Number(2), Number(3)), Number(4))
println("Expr2: " + show(expr2))
assert(eval(expr2) == 20)

val expr3 = Product(Number(2), Sum(Number(3), Number(4)))
println("Expr3: " + show(expr3))
assert(eval(expr3) == 14)

//Question 2

sealed abstract class BinaryTree

case class Leaf(value: Int) extends BinaryTree

case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree


val tree = Node(Node(Node(Leaf(4), Leaf(2)), Node(Leaf(1), Leaf(3))), Node(Node(Leaf(-1), Leaf(6)), Node(Leaf(0), Leaf(3))))

def sumOfTheLeaves(tree: BinaryTree): Int = tree match {
  case Leaf(value) => value
  case Node(left, right) => sumOfTheLeaves(left) + sumOfTheLeaves(right)
}

sumOfTheLeaves(tree)


def smallest(tree: BinaryTree): Int = tree match {
  case Node(Leaf(a), Leaf(b)) => Math.min(a, b)
  case Leaf(a) => a
  case Node(x, Leaf(c)) => Math.min(smallest(x), c)
  case Node(Leaf(c), x) => Math.min(smallest(x), c)
  case Node(a, b) => Math.min(smallest(a), smallest(b))
}

smallest(tree)
//Question 3

val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val ys = xs.map(x => x + 10)

def last[A](xs: List[A]): A = xs match {
  case Nil => throw new RuntimeException("Can't call last on empty list")
  case head :: Nil => head
  case _ :: tail => last(tail)
}

last(xs)

def init[A](xs: List[A]): List[A] = xs match {
  case Nil => Nil
  case _ :: Nil => Nil
  case head :: tail => head :: init(tail)
}

init(xs)

def reverse[A](xs: List[A]): List[A] = {
  def rec(currentList: List[A], remainingList: List[A]): List[A] = remainingList match {
    case Nil => Nil
    case head :: Nil => head :: currentList
    case head :: tail => rec(head :: currentList, tail)
  }

  rec(Nil, xs)
}

reverse(xs)

def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
  case Nil => ys
  case head :: tail => head :: concat(tail, ys)
}

concat(xs, ys)

def take[A](xs: List[A], n: Int): List[A] = n match {
  case x if x < 0 => throw new RuntimeException("Can't have negative n")
  case 0 => Nil
  case _ => xs match {
    case Nil => Nil
    case head :: tail => head :: take(tail, n - 1)
  }
}

take(xs, 2)
take(xs, 12)
take(xs, 0)
take(xs, 4)

def drop[A](xs: List[A], n: Int): List[A] = n match {
  case x if x < 0 => throw new RuntimeException("Can't have negative n")
  case 0 => xs
  case _ => xs match {
    case Nil => Nil
    case _ :: tail => drop(tail, n - 1)
  }
}

drop(xs, 2)
drop(xs, 12)
drop(xs, 0)

def apply[A](xs: List[A], n: Int): A = n match {
  case x if x < 0 => throw new RuntimeException("Can't have negative n")
  case 0 => xs.head
  case _ => xs match {
    case Nil => throw new RuntimeException("n bigger than size of list")
    case _ :: tail => apply(tail, n - 1)
  }
}


0.until(xs.length).foreach(n => println(apply(xs, n)))
//Complexity of last: n: size of list, O(n)
//Complexity of concat: n: size of first list, m: size of second, O(n)
//Complexity of reverse: n: size of the list, O(n)

//Question 4

def any[T](p: T => Boolean)(l: List[T]): Boolean = l match {
  case Nil => false
  case head :: _ if p(head) => true
  case _ :: tail => any(p)(tail)
}

any((x: Int) => x > 10)(xs)
any((x: Int) => x % 2 == 0)(xs)

def every[T](p: T => Boolean)(l: List[T]): Boolean = l match {
  case Nil => false
  case head :: Nil => p(head)
  case head :: _ if !p(head) => false
  case _ :: tail => every(p)(tail)
}

every((x: Int) => x > 10)(ys)
every((x: Int) => x % 2 == 0)(ys)