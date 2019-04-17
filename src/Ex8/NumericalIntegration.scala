package Ex8

import utils._


object NumericalIntegration extends App {
  def trapezArea(y0: Double, y1: Double, intervalSize: Double) = ((y0 + y1) * intervalSize) / 2

  def integrate(a: Double, b: Double, nIntervals: Double, f: Double => Double): Double = {
    val intervalSize = (b - a) / nIntervals

    val range = a.until(b).by(intervalSize)
    range.fold(0.0)((acc, current) => acc + trapezArea(f(current), f(current + intervalSize), intervalSize))
  }

  def integrateLazy(a: Double, b: Double, nIntervals: Double, f: Double => Double): Double = {
    val intervalSize = (b - a) / nIntervals

    val range = a.until(b).by(intervalSize).view
    range.fold(0.0)((acc, current) => acc + trapezArea(f(current), f(current + intervalSize), intervalSize))
  }

  def integratePar(a: Double, b: Double, nIntervals: Double, f: Double => Double): Double = {
    val intervalSize = (b - a) / nIntervals

    val range = a.until(b).by(intervalSize).par
    range.fold(0.0)((acc, current) => acc + trapezArea(f(current), f(current + intervalSize), intervalSize))
  }

  def integrateLazyPar(a: Double, b: Double, nIntervals: Double, f: Double => Double): Double = {
    val intervalSize = (b - a) / nIntervals

    val range = a.until(b).by(intervalSize).view.par
    range.fold(0.0)((acc, current) => acc + trapezArea(f(current), f(current + intervalSize), intervalSize))
  }


  val INTERVALS = Math.pow(20, 6)
  val sin = (x: Double) => Math.sin(x)
  val cos = (x: Double) => Math.cos(x)
  val f = sin compose cos
  println(integrate(0, 1, 10000, f))
  val timeIntegration = time {
    integrate(0, 1, INTERVALS, f)
  }
  val timeIntegrationLazy = time {
    integrateLazy(0, 1, INTERVALS, f)
  }
  val timeIntegrationPar = time {
    integratePar(0, 1, INTERVALS, f)
  }
  val timeIntegrationLazyPar = time {
    integrateLazyPar(0, 1, INTERVALS, f)
  }
  println(s"Standard: $timeIntegration ms")
  println(s"Lazy: $timeIntegrationLazy ms")
  println(s"Parallel: $timeIntegrationPar ms")
  println(s"Lazy parallel: $timeIntegrationLazyPar ms")
}
