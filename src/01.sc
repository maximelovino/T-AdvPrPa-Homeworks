// Question 1

def foo = 5.0
def bar = 3
def sum(x: Double, y: Double) = x + y

def square(x: Double) = x * x

def fourth(x: Double) = square(square(x))

square(2)
fourth(2)

// Question 2
def EPSILON = 0.0001

def isGoodEnough(x: Double, root: Double) = Math.abs(Math.sqrt(x) - root) < EPSILON

def f(approx: Double, x: Double) = square(approx) - x

def fPrime(approx: Double, x: => Double) = 2 * approx

def improve(x: Double, rootApprox: Double) = rootApprox - f(rootApprox, x) / fPrime(rootApprox, x)

def sqrt(x: Double): Double = {
  def loop(x: Double, approx: Double): Double = if (isGoodEnough(x, approx)) approx else loop(x, improve(x,approx))

  loop(x,10.0)
}

sqrt(9)
sqrt(16)

// Optional : Cubic root