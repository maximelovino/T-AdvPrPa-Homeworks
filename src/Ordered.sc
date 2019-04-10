case class Foo(x: Int) extends Ordered[Foo] {
  override def compare(that: Foo) = x - that.x
}

val a = Foo(2)
val b = Foo(3)
val c = Foo(2)

a < b
a > b
a == b
a == c
a >= c
