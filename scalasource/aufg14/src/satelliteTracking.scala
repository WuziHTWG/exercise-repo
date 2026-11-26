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

    def iss: LazyList[IssReading] = LazyList.continually { Thread.sleep(400) ; fetch() }
    val velocities = iss.map(_.velocity)
    val altitues = iss.map(_.altitude)

    val avVelocity = velocities.sliding(5).map(w => w.sum / w.length)
    val avAltitude = altitues.sliding(5).map(w => w.sum / w.length)

    def distance (from: IssReading, to: IssReading): Double= 

        import scala.math._
        val R = 6372.8 + to.altitude// Earth radius in kilometers
        val dLat: Double = toRadians((to.lat - from.lat))
        val dLon: Double = toRadians((to.lon - from.lon))
        
        val a = pow(sin(dLat / 2), 2) + cos(toRadians(from.lat)) * cos(toRadians(to.lat)) * pow(sin(dLon / 2), 2)
        
        val c = 2 * atan2(sqrt(a), sqrt(1 - a))
        R * c

    val distances: LazyList[Double] = iss.sliding(2).map(w => distance(w(0), w(1))).to(LazyList)


package HUBBLE: // Currently ISSUE with API, Worked Sat 21.11.26 befor 12:30, fixed trought other API
    val apiKey = sys.env.getOrElse(
        "N2YO_API_KEY",
        throw new RuntimeException("N2YO_API_KEY fehlt!")
    )
    val url = "https://api.n2yo.com/rest/v1/satellite/positions/20580/47.75/8.84/500/1/&apiKey="+ s"$apiKey"
    val json = Source.fromURL(url).mkString

    case class HubbleReading(time: Long, lat: Double, lon: Double, altitude: Double, velocity: Double);

    def field(json: String, name: String): Double = // minimal JSON parsing
        raw""""$name":(-?[\d.]+)""".r.findFirstMatchIn(json).get.group(1).toDouble
    def fetch(): HubbleReading =
        val json = Source.fromURL(url).mkString
        HubbleReading(field(json, "timestamp").toLong, field(json, "satlatitude"),
                    field(json, "satlongitude"), field(json, "sataltitude"), 0)

    def hubble: LazyList[HubbleReading] = LazyList.continually { Thread.sleep(500); fetch() }


@main def start: Unit =
    //ISS.iss.zip(HUBBLE.hubble).take(10).foreach((iss, hubble)=> println("ISS Altitude: "+ iss.altitude + "\n"+ "Hubble Space Telescope Altitude: " +  hubble.altitude))
    //ISS.iss.take(20).foreach(iss => println("ISS Altitude: " + BigDecimal(iss.altitude).setScale(7, BigDecimal.RoundingMode.HALF_UP).toString + " km"))
    //ISS.avAltitude.take(5).foreach(println)
    //ISS.avVelocity.take(5).foreach(println)
    ISS.iss.take(10).zip(ISS.distances).foreach((iss, distances) => println("ISS Geschwindigkeit: " + iss.velocity + " km/s\nBerechnete Distanz: "+ distances + " km in Readcycle (manchmal buggy) in sec"))