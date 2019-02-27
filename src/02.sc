import scala.annotation.tailrec

//Question 1a
def fact(x: Int): Int = {
  @tailrec
  def factTail(x: Int, h: Int): Int = if (x == 0) h else factTail(x - 1, x * h)

  factTail(x, 1)
}

fact(5)

//Question 1b

def fibStd(x: Int): Int = {
  if (x == 0 || x == 1) x
  else fibStd(x - 1) + fibStd(x - 2)
}

fibStd(8)

def fib(x: Int): Int = {
  @tailrec
  def fibTail(x: Int, twoStepsBehind: Int, oneStepBehind: Int): Int = {
    if (x == 0) twoStepsBehind else if (x == 1) oneStepBehind
    else fibTail(x - 1, oneStepBehind, twoStepsBehind + oneStepBehind)
  }

  fibTail(x, 0, 1)
}

fib(8)

//Question 2a

def sum(f: Double => Double, a: Int, b: Int) = {
  @tailrec
  def iter(a: Int, acc: Double): Double = {
    if (a > b) acc
    else iter(a + 1, f(a) + acc)
  }

  iter(a, 0)
}


sum((x: Double) => x * x, 1, 4)

//Question 3
