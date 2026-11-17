class Auto(var tankladung: Double, val farbe: String, val maxPs: Int, val verbrauchProKm: Double):
    def fahre(km: Int): Double=
       tankladung = tankladung - ( km * verbrauchProKm )
       tankladung

    def tankAnzeigen(): Unit =
        println(tankladung)
