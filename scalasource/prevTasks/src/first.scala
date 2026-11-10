@main def main(): Unit =
    //println(s"Hallo $name")
    /*val auto1 = new Auto(100, "Blau", 150, 0.1)
    
    val auto2 = new Auto(80, "Rot", 150, 0.07)


    auto1.fahre(20)

    auto2.fahre(35)

    auto1.tankAnzeigen()
    auto2.tankAnzeigen() */
    val c = 51966.toBinaryString
    val d = 51966.toHexString
    println(c)
    println(d)
    println(Int.MaxValue + 1)
    println(0.1+0.2)
    var a: BigInt = 1
    for (i <- 1 to 25) {  
    a = a * i
    }
    println(a)
    println((1 to 20 by 4).toList)
