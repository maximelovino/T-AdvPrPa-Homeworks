import scala.annotation.tailrec

def patFoo(x: Any): Boolean = x match {
  case a: Int if a % 4 == 0 => true
  case b: String if b.length == 1 && b.charAt(0).isUpper => true
  case b: Char if b.isUpper => true
  case _: Boolean => true
  case _ => false
}

patFoo(3)
patFoo(4)
patFoo("hello")
patFoo("Hello")
patFoo(true)
patFoo(false)
patFoo(3.1)

def insert(elem: Int, l: List[Int]): List[Int] = {
  l match {
    case Nil => elem :: Nil
    case head :: tail => {
      if (elem < head) {
        elem :: l
      } else {
        head :: insert(elem, tail)
      }
    }
  }
}

def isort(xs: List[Int]): List[Int] =
  if (xs.isEmpty) Nil else insert(xs.head, isort(xs.tail))

insert(3, Nil)
insert(3, 4 :: Nil)
insert(3, 2 :: Nil)

isort(List(7, 3, 9, 2))


def length[A](x: List[A]): Int = {

  (x foldRight 0) ((_, y) => y + 1)

}

def map[A, B](x: List[A], f: A => B): List[B] = {

  (x foldRight List.empty[B]) ((a, b) => f(a) :: b)

}

val xs = List(1, 2, 3, 4)

length(xs)
map(xs, (x: Int) => x * 2)

def dup[A](xs: List[A]): List[A] = {
  xs.foldRight(List.empty[A])((a, b) => a :: a :: b)
}

dup(xs)

def filter[A](f: A => Boolean, b: List[A]) = {
  b.foldRight(List.empty[A])((a, b) => if (f(a)) {
    a :: b
  } else {
    b
  })
}

val b = (1 to 10).toList
filter((a: Int) => a % 2 == 0, b)

def partition[A](f: A => Boolean, b: List[A]) = {
  b.foldRight((List.empty[A], List.empty[A]))((a, b) => if (f(a)) {
    (a :: b._1, b._2)
  } else {
    (b._1, a :: b._2)
  })
}

partition((a: Int) => a % 2 == 0, b)


case class User(name: String, age: Int)

val userBase = List(User("Travis", 28),
  User("Kelly", 33),
  User("Jennifer", 44),
  User("Dennis", 23))

val youngNames = for (
  u <- userBase
  if u.age < 30
) yield u.name

println(youngNames)
