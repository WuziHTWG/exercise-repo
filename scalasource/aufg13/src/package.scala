//Funktioniert entgegen dem was Boger gesagt hat nicht
////> using dep org.typelevel::squants:1.8.3
import energy.Units.min

package energy:
    import squants.market.* 
    import squants.energy.*
    import squants.time.*
    object Units:
        extension (n: Int)
            def kWh: Energy = KilowattHours(n)
            def kW: Power = Kilowatts(n)
            def hours: Time = Hours(n)
            //def eur: Money = EUR(n)
        extension (d: Double)
            def eur: Money = EUR(d)
            def usd: Money = USD(d)
            def chf: Money = CHF(d)
            def MWh: Energy = MegawattHours(d)
            def min: Time = Minutes(d)
            def gW: Power = Gigawatts(d)
            def sec: Time = Seconds(d)

package billing:
    import squants.energy.MegawattHours
    
    @main def main (): Unit = 
        // First und second sind selbst geschrieben
        {
        def first: Unit =
            import energy.Units.*
            println(10.eur + eur(10))
            println(3500.0.MWh)
            println((10.gW * 60.min).in(MegawattHours))
        first
        def second: Unit =
            import energy.Units.{kWh as KWH, eur as Eur, gW as GW, min}
            println((10.Eur + Eur(10)))
            println(3500.KWH)
            println((10.GW * 60.min).in(MegawattHours))
        second
        }

        import squants.market.* 
        import energy.Units.* 
        import squants.energy.{Energy, KilowattHours, MegawattHours}

        object Metering:
            opaque type Reading = Energy // inside Metering: Reading == Energy
            object Reading:
                def apply(kWh: Double): Reading =
                    require(kWh >= 0, "a meter reading is never negative")
                    KilowattHours(kWh)
                extension (r: Reading)
                    def since(earlier: Reading): Energy = r - earlier

        import Metering.*
        given MoneyContext(EUR, defaultCurrencySet, List(CurrencyExchangeRate(EUR(1), USD(1.10))))
        val jan = Reading(12_400)
        val dec = Reading(15_900)
        dec.since(jan)
        // Ende von Kopiertem Code
        val p: Price[Energy] = 0.35.usd / 1.kWh
        
        val usage = (2.gW * 30.min).in(MegawattHours)
        val baseFee = 12.eur * 12
        val total = ((p * usage).in(EUR) + baseFee).rounded(2)
        def calcEnergyBillInEur(Reading1: Reading, Reading2: Reading, price: Price[Energy], baseFee: Double): Money =
            ((price * (Reading2.since(Reading1))).in(EUR) + baseFee.eur * 12).rounded(2)
        val PriceInEur = calcEnergyBillInEur(Reading(12400), Reading(15900), p, 12)
        val PriceInUSD = PriceInEur.in(USD)
        println(PriceInUSD)
        println(PriceInEur)