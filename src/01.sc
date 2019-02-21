// Question 1

def foo = 5.0
def bar = 3
def sum(x: Double, y: Double) = x + y

def square(x: Double) = x * x

def third(x: Double) = square(x) * x

def fourth(x: Double) = square(square(x))

square(2)
third(2)
fourth(2)

// Question 2
def EPSILON = 0.0001

//Square root newton
def f(approx: Double, x: Double) = square(approx) - x
def fPrime(approx: Double) = 2 * approx

//Cubic root newton
def fThird(approx: Double, x: Double) = third(approx) - x
def fPrimeThird(approx: Double) = 3 * square(approx)

def root(x: Double, precision: Double, f: (Double, Double) => Double, fPrime: Double => Double, mathLibMethod: Double => Double): Double = {

  def isGoodEnough(x: Double, approx: Double) = Math.abs(mathLibMethod(x) - approx) < precision

  def improve(x: Double, approx: Double) = approx - f(approx,x)/fPrime(approx)

  def loop(x: Double, approx: Double): Double = if (isGoodEnough(x, approx)) approx else loop(x, improve(x, approx))

  loop(x, 10.0)
}

root(27,EPSILON,fThird,fPrimeThird,Math.cbrt)
root(9,EPSILON,f,fPrime,Math.sqrt)
