//> using dep org.typelevel::squants:1.8.3
        // First und second sind selbst geschrieben
        /*def first: Unit =
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
        */
        import squants.market.* 
        import energy.Units.* 
        import squants.energy.{Energy, KilowattHours}
        import squants.energy.MegawattHours
        import scala.caps.Read
        object Metering:
            opaque type Reading = Energy // inside Metering: Reading == Energy
            object Reading:
                def apply(kWh: Double): Reading =
                    require(kWh >= 0, "a meter reading is never negative")
                    KilowattHours(kWh)
                extension (r: Reading)
                    def since(earlier: Reading): Energy = r - earlier

        import Metering.*
        val jan = Reading(12_400)
        val dec = Reading(15_900)
        dec.since(jan)
        // Ende von Kopiertem Code
        val p: Price[Energy] = 0.35.usd / 1.kWh
        given MoneyContext(EUR, defaultCurrencySet, List(CurrencyExchangeRate(EUR(1), USD(1.10))))
        val usage = (2.gW * 30.min).in(MegawattHours)
        val baseFee = 12.eur * 12
        val total = ((p * usage).in(EUR) + baseFee).rounded(2)
        def calcEnergyBillInEur(Reading1: Reading, Reading2: Reading, price: Price[Energy], baseFee: Double): Money =
            ((price * (Reading2.since(Reading1))).in(EUR) + baseFee.eur * 12).rounded(2)
        val PriceInEur = calcEnergyBillInEur(Reading(12400), Reading(15900), p, 12)
        val PriceInUSD = PriceInEur.in(USD)