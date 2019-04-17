package Ex8

import java.net.URL

import net.liftweb.json.{DefaultFormats, parse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object BitcoinAPI extends App {
  implicit val formats: DefaultFormats.type = DefaultFormats

  def contentFromURL(url: String): Future[String] = Future {
    val source = Source.fromURL(new URL(url))
    source.mkString
  }

  val btcUsdFuture = contentFromURL("https://apiv2.bitcoinaverage.com/indices/global/ticker/BTCUSD").map(parse)
  val btcEurFuture = contentFromURL("https://apiv2.bitcoinaverage.com/indices/global/ticker/BTCEUR").map(parse)

  val usdEurRate = for {
    btcUsd <- btcUsdFuture
    btcEur <- btcEurFuture
  } yield {
    val usd = (btcUsd \ "bid").extract[Double]
    val eur = (btcEur \ "bid").extract[Double]
    eur / usd
  }

  try {
    val rate = Await.result(usdEurRate, Duration.Inf)
    println(s"1 USD = $rate EUR")
  } catch {
    case exception: Exception => println(s"There was the following problem: ${exception.getMessage}")
  }
}
