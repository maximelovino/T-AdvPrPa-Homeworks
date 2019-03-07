class Rational(n: Int, d: Int) {
  require(d > 0, "Don't do that you little bitch")

  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  private val g = gcd(n, d)


  def num = n / g

  def denom = d / g

  def +(that: Rational) = {
    new Rational(num * that.denom + that.num * denom, denom * that.denom)
  }

  def -(that: Rational) = {
    new Rational(num * that.denom - that.num * denom, denom * that.denom)
  }

  def *(that: Rational) = {
    new Rational(num * that.num, denom * that.denom)
  }

  def /(that: Rational) = {
    new Rational(num * that.denom, denom * that.num)
  }

  def ==(that: Rational) = {
    num * that.denom == denom * that.num
  }

  def unary_-(): Rational = {
    new Rational(-1 * num, denom)
  }

  def <(that: Rational): Boolean = {
    num * that.denom < denom * that.num
  }

  def max(that: Rational): Rational = {
    if (this < that)
      that
    else
      this
  }

  override def toString() = num + "/" + denom
}

val r1 = new Rational(4, 6)
val r2 = new Rational(10, 20)

r1 < r2
r2 < r1
r1.max(r2)
-r1