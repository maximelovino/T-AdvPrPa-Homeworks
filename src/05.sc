/*
PART A
 */

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

def isPresent(xs: List[Int], element: Int): Boolean = xs.foldLeft(false)((a, b) => a || b == element)

isPresent(List(1, 2, 3, 4), 5)
isPresent(List(1, 2, 3, 4), 3)

// Not perfect because not tail rec
def flattenList(xs: List[Any]): List[Any] = {
  xs.foldRight(List.empty[Any])((a, b) => a match {
    case x: List[Any] => flattenList(x) ++ b
    case _ => a :: b
  })
}

flattenList(List(List(1, 1), 2, List(3, List(5, 8))))

/*
PART B
 */

// Question 1

def isPrime(i: Int): Boolean = i match {
  case i if i <= 1 => false
  case 2 => false
  case _ => !(2 until i).exists(x => i % x == 0)
}

def primeSum(max: Int): List[(Int, Int)] = for {
  i <- (1 to max).toList
  j <- (1 to max).toList
  if isPrime(i + j)
} yield (i, j)

val primeSumList = primeSum(10)
println(s"Prime sum with dup size: ${primeSumList.size}")
def dedupPattern(xs: List[(Int, Int)]): List[(Int, Int)] = {
  def rec(current: List[(Int, Int)], remaining: List[(Int, Int)]): List[(Int, Int)] = remaining match {
    case Nil => current
    case (a, b) :: tail if current.contains((b, a)) => rec(current, tail)
    case head :: tail => rec(head :: current, tail)
  }

  rec(Nil, xs)
}

val primeSumListDedupPattern = dedupPattern(primeSumList)
println(s"Prime sum with pattern dedup size: ${primeSumListDedupPattern.size}")

//Note that the elements kept will not be the same between dedupFold and dedupPattern to the iteration order difference between the two
def dedupFold(xs: List[(Int, Int)]): List[(Int, Int)] = {
  xs.foldRight(List.empty[(Int, Int)])((tuple, list) => if (list.contains((tuple._2, tuple._1))) list else tuple :: list)
}

val primeSumListDedupFold = dedupFold(primeSumList)
println(s"Prime sum with fold dedup size: ${primeSumListDedupFold.size}")

// Question 2

val cities = List("Paris", "London", "Berlin", "Lausanne")
val relatives = List("Grandma", "Grandpa", "Aunt Lottie", "Dad")
val travellers = List("Pierre-Andre", "Rachel")


val postcardsAll = for {
  traveller <- travellers
  relative <- relatives
  city <- cities
} yield s"Dear $relative, Wish you were here in $city! Love, $traveller"

postcardsAll.foreach(println)

val postcardsG = for {
  traveller <- travellers
  relative <- relatives
  city <- cities
  if relative.startsWith("G")
} yield s"Dear $relative, Wish you were here in $city! Love, $traveller"

postcardsG.foreach(println)


// Question 3

def inCheck(q1: (Int, Int), q2: (Int, Int)) =
  q1._1 == q2._1 || // same row
    q1._2 == q2._2 || // same column
    (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal

def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) =
  queens forall (q => !inCheck(queen, q))

def queens(n: Int): List[List[(Int, Int)]] = {

  def placeQueens(k: Int): List[List[(Int, Int)]] =
    if (k == 0)
      List(List())
    else
      for {
        queens <- placeQueens(k - 1)
        column <- 1 to n
        queen = (k, column)
        if isSafe(queen, queens)
      } yield queen :: queens

  placeQueens(n)

}

//TODO redo it using for comprehension
def printChessBoard(queens: List[List[(Int, Int)]]): String = {
  queens.map(queenSolution => {
    (1 to queenSolution.size).foldLeft("")((checkers: String, y: Int) => {
      checkers + (1 to queenSolution.size).foldLeft("|")((line: String, x: Int) => if (queenSolution.contains((x, y))) line + "\u265b|" else line + "_|") + "\n"
    })
  }).zipWithIndex.map {
    case (solution, index) => s"Solution ${index + 1}:\n$solution"
  }.mkString("\n\n")
}
val solutions = queens(4)
println(printChessBoard(solutions))

//printChessBoard(solutions).foreach(println)

