import scala.io.Source
import scala.util.Using
import scala.math.BigDecimal


package ISS:
    /* 
    Velocity in km/s; Division neccesary for ISS, they return value in km/h
     */

    val url = "https://api.wheretheiss.at/v1/satellites/25544"
    val json = Source.fromURL(url).mkString

    case class IssReading(time: Long, lat: Double, lon: Double, altitude: Double, velocity: Double);

    def field(json: String, name: String): Double = // minimal JSON parsing
        raw""""$name":(-?[\d.]+)""".r.findFirstMatchIn(json).get.group(1).toDouble
    def fetch(): IssReading =
        val json = Source.fromURL(url).mkString
        IssReading(field(json, "timestamp").toLong, field(json, "latitude"),
                    field(json, "longitude"), field(json, "altitude"), field(json, "velocity")/3600)

    def iss: LazyList[IssReading] = LazyList.continually { Thread.sleep(1000); fetch() }

package HUBBLE: // Currently ISSUE with API, Worked Sat 21.11.26 befor 12:30 
    import java.time.Instant

    val url = "https://satellites.orbitingfrog.com/api/positions?id=20580"
    val json = Source.fromURL(url).mkString

    case class HubbleReading(time: Long, lat: Double, lon: Double, altitude: Double, velocity: Double);
    def timestamp: Long= Instant.now().getEpochSecond

    def field(json: String, name: String): Double = // minimal JSON parsing
        raw""""$name":(-?[\d.]+)""".r.findFirstMatchIn(json).get.group(1).toDouble
    def fetch(): HubbleReading =
        val json = Source.fromURL(url).mkString
        HubbleReading(timestamp, field(json, "lat"),
                    field(json, "lon"), field(json, "alt"), field(json, "velocity"))

    def hubble: LazyList[HubbleReading] = LazyList.continually { Thread.sleep(1000); fetch() }

@main def start: Unit =
    //ISS.iss.zip(HUBBLE.hubble).take(20).foreach((iss, hubble)=> println("ISS Altitude: "+ iss.altitude + "\n"+ "Hubble Space Telescope Altitude: " +  hubble.altitude))
    ISS.iss.take(20).foreach(iss => println("ISS Altitude: " + BigDecimal(iss.altitude).setScale(7, BigDecimal.RoundingMode.HALF_UP).toString + " km"))