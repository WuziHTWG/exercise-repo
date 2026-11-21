import scala.io.Source
import scala.util.Using

val url = "https://api.wheretheiss.at/v1/satellites/25544"
val json = Source.fromURL(url).mkString

case class IssReading(time: Long, lat: Double, lon: Double, altitude: Double, velocity: Double)
    def field(json: String, name: String): Double = // minimal JSON parsing
        raw""""$name":(-?[\d.]+)""".r.findFirstMatchIn(json).get.group(1).toDouble
    def fetch(): IssReading =
        val json = Source.fromURL(url).mkString
        IssReading(field(json, "timestamp").toLong, field(json, "latitude"),
        field(json, "longitude"), field(json, "altitude"), field(json, "velocity"))
    fetch()

var requests = 0

def counted(): IssReading = { requests += 1; fetch() } // fetch() from the previous slide

val iss: LazyList[IssReading] = LazyList.continually { Thread.sleep(1000); counted() }
