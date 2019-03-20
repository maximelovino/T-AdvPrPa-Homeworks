// Question 1

def lengthStrings(xs: List[String]): List[Int] = xs.map(x => x.length)

def dup[A](xs: A, n: Int): List[A] = 0.until(n).toList.map(_ => xs)

def dot(a: List[Int], b: List[Int]): List[Int] = (a zip b).map(x => x._1 * x._2)

lengthStrings(List("How", "long", "are", "we?"))
dup("foo", 5)
dup(List(1, 2, 3), 2)
dot(List(1, 2, 3), List(2, 4, 3))

// Question 2
def areTrue(xs: List[Boolean]): Boolean = xs.foldLeft(true)((a, b) => a && b)

areTrue(List(true, true, false))
areTrue(List(true, true, true))

def lString(xs: List[String]): Int = xs.foldLeft(0)((a, b) => a + b.length)

lString(List("Folding", "is", "fun"))

def longest(xs: List[String]): (Int, String) = xs.foldLeft((0, ""))((a, b) => if (b.length > a._1) (b.length, b) else a)

longest(List("What", "is", "the", "longest?"))

// Not perfect because not tail rec
def flattenList(xs: List[Any]): List[Any] = {
  xs.foldRight(List.empty[Any])((a, b) => a match {
    case x: List[Any] => flattenList(x) ++ b
    case _ => a :: b
  })
}

flattenList(List(List(1, 1), 2, List(3, List(5, 8))))

