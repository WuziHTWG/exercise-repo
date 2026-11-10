// ==================================================
// MAIN.SCALA - EINGABEN UND EINSTELLUNGEN
// ==================================================

@main def gates(): Unit =

  // Eingangsvariablen
  val vars = List("A", "B", "C", "D", "E")

  // EINGABEMODUS:
  // 0 = Eingangsgleichungen eingeben
  // 1 = Funktionswerte (Wahrheitstabellen) eingeben
  val inputMode = 1

  // GATTERMODUS:
  // 0 = Keine zusaetzliche Gatterdarstellung
  // 1 = Ausschliesslich zweistellige NAND-Gatter
  // 2 = Ausschliesslich zweistellige NOR-Gatter
  val gateMode = 0

  // OPTION A: EINGANGSGLEICHUNGEN
  // Wird nur verwendet, wenn inputMode = 0.

  val functions = List(
    BoolFunction(
      "f1",
      x =>
        gatesUtils.or(
          gatesUtils.and(x("A"), x("B")),
          x("C")
        )
    ),
    BoolFunction(
      "f2",
      x =>
        gatesUtils.xor(
          x("A"),
          x("B")
        )
    ),
    BoolFunction(
      "f3",
      x =>
        gatesUtils.nand(
          x("A"),
          x("B")
        )
    ),
    BoolFunction(
      "f4",
      x =>
        gatesUtils.or(
          gatesUtils.and(x("A"), x("B")),
          gatesUtils.and(x("B"), x("C"))
        )
    ),
    BoolFunction(
      "f5",
      x =>
        gatesUtils.xor(
          gatesUtils.nand(x("A"), x("B")),
          gatesUtils.nor(x("B"), x("C"))
        )
    )
  )

  // OPTION B: FUNKTIONSWERTE
  // Wird nur verwendet, wenn inputMode = 1.
  // Reihenfolge bei A, B, C:
  // 000, 001, 010, 011, 100, 101, 110, 111
  // Bei vier Variablen entsprechend: 0000 bis 1111.

  val truthTables = List(
    (
      "f1",
      List(0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1)
    )
    /*(
      "f2",
      List(0, 1, 1, 0, 1, 0, 0, 1)
    ),
    (
      "f3",
      List(1, 1, 1, 0, 1, 1, 1, 0)
    )*/
  )

  // Berechnung mit den oben festgelegten Eingaben starten.
  measureTime {
    BooleanCalculator.run(vars, inputMode, gateMode, functions, truthTables)
  }
