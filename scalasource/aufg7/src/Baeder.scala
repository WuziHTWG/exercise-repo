@main def main(): Unit =

  val besucherKinderbecken = Array(10, 15, 20)
  val temperaturenKinderbecken = List(20.0, 21.0, 22.0)
  val kinderbecken = Becken(temperaturenKinderbecken, besucherKinderbecken) // Eingabe von Variablen in ein Objekt
  val spaßbecken = Becken(List(25.0, 26.0, 27.0), Array(30, 35, 40))
  val bahnenbecken = Becken(List(28.0, 29.0, 30.0), Array(50, 55, 60))
  val nichtschwimmerbecken = Becken(List(24.0, 25.0, 26.0), Array(20, 25, 30))

  println("Kinderbecken: " + kinderbecken.durchschnittlicheBesucherzahl())
  println({
    kinderbecken.durchschnittlicheTemperatur()
  })
  // Folgende die Ausgabe der Wert die zuvor durch Methoden der Klasse Becken berechnet wurden, mit Inline-Berechnungen.
  println("Kinderbecken: " + {
    var summe = 0.0; for i <- besucherKinderbecken do summe += i; summe / besucherKinderbecken.length
  })
  println({ var summe = 0.0; for i <- temperaturenKinderbecken do summe += i; summe / temperaturenKinderbecken.length })
