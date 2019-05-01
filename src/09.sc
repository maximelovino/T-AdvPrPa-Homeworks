sealed trait Temperature {
  val value: Double
}

case class Celsius(value: Double) extends Temperature {
  require(value >= -273.15, "Celsius can't be lower than 273.15")

  override def toString: String = s"$value C"
}


case class Kelvin(value: Double) extends Temperature {
  require(value >= 0, "Kelvin can't be lower than 0")

  override def toString: String = s"$value° K"
}

object Temperature {
  //We could do double to celsius and kelvin and the int below would be promoted but it doesn't work in worksheets
  implicit def intToCelsius(value: Int): Celsius = Celsius(value)

  implicit def intToKelvin(value: Int): Kelvin = Kelvin(value)

  implicit def celsiusToKelvin(value: Celsius): Kelvin = Kelvin(value.value + 273.15)

  implicit def kelvinToCelsius(value: Kelvin): Celsius = Celsius(value.value - 273.15)
}


import Temperature._


val a: Celsius = 30
val b: Kelvin = 30
val c: Kelvin = Celsius(10)
val d: Celsius = c
val e: Temperature = d

println(a) // Should print "30 ◦ C"
println(b) // Should print "30 K"
println(e)

