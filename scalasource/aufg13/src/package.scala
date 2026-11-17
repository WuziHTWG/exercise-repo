//Funktioniert entgegen dem was Boger gesagt hat nicht
////> using dep org.typelevel::squants:1.8.3

import squants.market.* 
import squants.energy.*
import squants.time.* 

package energy:


    object Units:
        extension (n: Int)
            def kWh: Energy = KilowattHours(n)
            def kW: Power = Kilowatts(n)
            def hours: Time = Hours(n)
            def eur: Money = EUR(n)
        extension (d: Double)
//            def eur: Money = EUR(d)
            def usd: Money = USD(d)
            def chf: Money = CHF(d)
            def MWh: Energy = MegawattHours(d)
            def min: Time = Minutes(d)
            def gW: Power = Gigawatts(d)
            def sec: Time = Seconds(d)

package billing:
    @main def main (): Unit = 
        def first: Unit =
            import energy.Units.*
            println((10.eur + eur(10)))
            println(3500.MWh)
            println((10.gW * 60.min).in(MegawattHours))
        first
        def second: Unit =
            import energy.Units.{kWh as KWH, eur as Eur, gW as GW, min}
            println((10.Eur + Eur(10)))
            println(3500.KWH)
            println((10.GW * 60.min).in(MegawattHours))
        second
