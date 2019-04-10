//Question 1 – A covariant stack

trait Stack[+A] {
  def push[B >: A](elem: B): Stack[B]

  def top: A

  def pop: Stack[A]
}

/*
Considering that the same person implements everything, where is the best location to implement the push method? Why?

Directly in Stack trait because it's the same for all implementations
 */

case class ElemStack[+A](elem: A, stack: Stack[A]) extends Stack[A] {
  override def push[B >: A](elem: B) = ElemStack(elem, this)

  override def top = elem

  override def pop = stack match {
    case EmptyStack() => EmptyStack()
    case ElemStack(head, tail) => ElemStack(head, tail)
  }

  override def toString = s"$elem,$stack"
}

case class EmptyStack[+A]() extends Stack[A] {
  override def push[B >: A](elem: B) = ElemStack(elem, this)

  override def top = {
    throw new RuntimeException("Can't take the top of empty stack")
  }

  override def pop = this
}

val a = EmptyStack().push("hello").push("world").push("it’s fun").pop
assert(a.toString == "world,hello,EmptyStack()")


val b = EmptyStack().push(1).push(3)
assert(b.top == 3)

class Foo

class Bar extends Foo

val c: Stack[Bar] = EmptyStack().push(new Bar()).push(new Bar())
assert(c.top.isInstanceOf[Bar])
assert(c.top.isInstanceOf[Foo])

val d: Stack[Foo] = EmptyStack().push(new Bar()).push(new Bar())
assert(d.top.isInstanceOf[Foo])


//Question 2 – The Fibonacci sequence using infinite streams

def addStream(s1: Stream[Int], s2: Stream[Int]): Stream[Int] = s1.zip(s2).map(s => s._1 + s._2)


lazy val fib: Stream[Int] = 0 #:: 1 #:: addStream(fib, fib.tail)

fib(8)

//Question 3 – Streams of prime numbers

/**
  * @param n The number of primes to return
  * @return A list of the first n primes
  */
def firstPrimes(n: Int): List[Int] = {
  def from(n: Int): Stream[Int] = n #:: from(n + 1)

  def loop(stream: Stream[Int], primes: List[Int], remaining: Int): List[Int] = remaining match {
    case 0 => primes
    case x => loop(stream.tail.filter(_ % stream.head != 0), stream.head :: primes, x - 1)
  }

  loop(from(2), Nil, n).reverse
}

firstPrimes(20)