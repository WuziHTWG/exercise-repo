import scala.collection.mutable

// ==================================================
// 1. GRUNDLEGENDE BOOLSCHE VERKNUEPFUNGEN
// ==================================================

object gatesUtils:

  def and(a: Boolean, b: Boolean): Boolean = a && b
  def or(a: Boolean, b: Boolean): Boolean = a || b
  def not(a: Boolean): Boolean = !a
  def xor(a: Boolean, b: Boolean): Boolean = a ^ b
  def xnor(a: Boolean, b: Boolean): Boolean = !(a ^ b)
  def nor(a: Boolean, b: Boolean): Boolean = !(a || b)
  def nand(a: Boolean, b: Boolean): Boolean = !(a && b)

// ==================================================
// 2. DATENTYPEN
// ==================================================
def measureTime[T](block: => T): T =
  val start = System.nanoTime()

  try block
  finally
    val end = System.nanoTime()
    val duration = (end - start) / 1_000_000.0
    println(s"Laufzeit: $duration ms")

case class BoolFunction(
    name: String,
    eval: Map[String, Boolean] => Boolean
)

case class Implicant(
    pattern: String,
    covered: Set[Int]
)

// ==================================================
// 3. NORMALFORMEN UND MINIMIERUNG
// ==================================================

object NormalformUtils:

  def binary(number: Int, n: Int): String =
    number.toBinaryString.reverse
      .padTo(n, '0')
      .reverse

  def minterm(vars: List[String], bits: String): String =
    vars
      .zip(bits)
      .map {
        case (v, '1') => v
        case (v, '0') => "¬" + v
        case _        => ""
      }
      .mkString("(", " x ", ")")

  def maxterm(vars: List[String], bits: String): String =
    vars
      .zip(bits)
      .map {
        case (v, '0') => v
        case (v, '1') => "¬" + v
        case _        => ""
      }
      .mkString("(", " + ", ")")

  // Zwei Implikanten kombinieren

  def combine(
      a: Implicant,
      b: Implicant
  ): Option[Implicant] =

    var differences = 0
    var result = ""
    var i = 0
    var compatible = true

    while i < a.pattern.length && compatible do
      val x = a.pattern(i)
      val y = b.pattern(i)

      if x == y then result += x
      else if x != '-' && y != '-' then
        differences += 1
        result += '-'
      else compatible = false

      i += 1

    if compatible && differences == 1 then Some(Implicant(result, a.covered ++ b.covered))
    else None

  // Primimplikanten berechnen

  def primeImplicants(
      minterms: Set[Int],
      n: Int
  ): List[Implicant] =

    var current = minterms.toList.map { m =>
      Implicant(binary(m, n), Set(m))
    }

    var primes = List.empty[Implicant]

    while current.nonEmpty do

      val used = mutable.Set[String]()
      val next = mutable.ListBuffer[Implicant]()

      for i <- current.indices do
        for j <- (i + 1) until current.length do

          combine(current(i), current(j)) match

            case Some(result) =>
              used += current(i).pattern
              used += current(j).pattern
              next += result

            case None => ()

      primes ++= current.filterNot(x => used.contains(x.pattern))

      current = next
        .groupBy(_.pattern)
        .map { case (pattern, terms) =>
          Implicant(
            pattern,
            terms.flatMap(_.covered).toSet
          )
        }
        .toList

    primes
      .groupBy(_.pattern)
      .map { case (pattern, terms) =>
        Implicant(
          pattern,
          terms.flatMap(_.covered).toSet
        )
      }
      .toList
      .sortBy(_.pattern)

  // Kleinste Ueberdeckung suchen

  def minimumCover(
      primes: List[Implicant],
      target: Set[Int]
  ): List[Implicant] =

    if target.isEmpty then return List.empty

    var best = List.empty[Implicant]
    var bestLiterals = Int.MaxValue
    var bestTerms = Int.MaxValue

    def search(
        start: Int,
        chosen: List[Implicant],
        covered: Set[Int]
    ): Unit =

      if target.subsetOf(covered) then

        val literals = chosen
          .map(
            _.pattern.count(_ != '-')
          )
          .sum

        val terms = chosen.length

        if literals < bestLiterals ||
          (literals == bestLiterals && terms < bestTerms)
        then
          best = chosen
          bestLiterals = literals
          bestTerms = terms
      else

        for i <- start until primes.length do
          search(
            i + 1,
            chosen :+ primes(i),
            covered ++ primes(i).covered
          )

    search(0, List.empty, Set.empty)
    best

  // Minimierte DNF

  def sop(
      vars: List[String],
      terms: List[Implicant]
  ): String =

    if terms.isEmpty then "0"
    else if terms.exists(_.pattern.forall(_ == '-')) then "1"
    else
      terms
        .map { term =>

          val literals = vars.zip(term.pattern).collect {
            case (v, '1') => v
            case (v, '0') => "¬" + v
          }

          literals.mkString("(", " x ", ")")

        }
        .mkString(" + ")

  // Minimierte KNF

  def pos(
      vars: List[String],
      terms: List[Implicant]
  ): String =

    if terms.isEmpty then "1"
    else if terms.exists(_.pattern.forall(_ == '-')) then "0"
    else
      terms
        .map { term =>

          val literals = vars.zip(term.pattern).collect {
            case (v, '0') => v
            case (v, '1') => "¬" + v
          }

          literals.mkString("(", " + ", ")")

        }
        .mkString(" x ")

// ==================================================
// 4. ZWEISTELLIGE GATTERERZEUGUNG
// ==================================================

class GateBuilder(mode: Int, vars: List[String]):

  private val lines = mutable.ListBuffer[String]()
  private var counter = 0

  private def addGate(
      gate: String,
      a: String,
      b: String
  ): String =

    counter += 1

    val result = "G" + counter

    lines += s"$result = $gate($a, $b)"

    result

  // Alle folgenden Methoden benutzen
  // ausschliesslich zweistellige Gatter.

  // NAND-Modus

  private def nand(a: String, b: String): String =
    addGate("NAND", a, b)

  private def nandNot(a: String): String =
    nand(a, a)

  private def nandAnd(a: String, b: String): String =
    nandNot(nand(a, b))

  private def nandOr(a: String, b: String): String =
    nand(
      nandNot(a),
      nandNot(b)
    )

  // NOR-Modus

  private def nor(a: String, b: String): String =
    addGate("NOR", a, b)

  private def norNot(a: String): String =
    nor(a, a)

  private def norOr(a: String, b: String): String =
    norNot(nor(a, b))

  private def norAnd(a: String, b: String): String =
    nor(
      norNot(a),
      norNot(b)
    )

  // Negierte Eingangsvariable erzeugen

  private def literal(
      variable: String,
      positive: Boolean
  ): String =

    if positive then variable
    else if mode == 1 then nandNot(variable)
    else norNot(variable)

  // Konstanten mit zweistelligen Gattern erzeugen

  private def constant(value: Boolean): String =

    val a = vars.head
    val notA = literal(a, false)

    if mode == 1 then

      val one = nand(a, notA)

      if value then one
      else nand(one, one)
    else

      val zero = nor(a, notA)

      if value then nor(zero, zero)
      else zero

  // DNF mit ausschliesslich NAND-Gattern

  def buildNandDNF(
      variables: List[String],
      terms: List[Implicant]
  ): String =

    if terms.isEmpty then return constant(false)

    if terms.exists(_.pattern.forall(_ == '-')) then return constant(true)

    // Jeden Produktterm mit zweistelligen
    // NAND-Gattern und Rueckinvertierung bilden.

    val products = terms.map { term =>

      val literals = variables.zip(term.pattern).collect {

        case (v, '1') => literal(v, true)
        case (v, '0') => literal(v, false)

      }

      if literals.isEmpty then constant(true)
      else literals.reduce(nandAnd)
    }

    // Die Produktterme mit zweistelligen
    // NAND-basierten OR-Verknuepfungen verbinden.

    products.reduce(nandOr)

  // KNF mit ausschliesslich NOR-Gattern

  def buildNorKNF(
      variables: List[String],
      terms: List[Implicant]
  ): String =

    if terms.isEmpty then return constant(true)

    if terms.exists(_.pattern.forall(_ == '-')) then return constant(false)

    // Jeden Summenterm mit zweistelligen
    // NOR-Gattern und Rueckinvertierung bilden.

    val clauses = terms.map { term =>

      val literals = variables.zip(term.pattern).collect {

        case (v, '0') => literal(v, true)
        case (v, '1') => literal(v, false)

      }

      if literals.isEmpty then constant(false)
      else literals.reduce(norOr)
    }

    // Die Summenterme mit zweistelligen
    // NOR-basierten AND-Verknuepfungen verbinden.

    clauses.reduce(norAnd)

  // Alle erzeugten Gatter ausgeben

  def output(result: String): Unit =

    lines.foreach(println)

    println("Ausgang = " + result)

// ==================================================
// BOOLEAN CALCULATOR - GESAMTE BERECHNUNGSLOGIK
// ==================================================

object BooleanCalculator:

  def run(
      vars: List[String],
      inputMode: Int,
      gateMode: Int,
      functions: List[BoolFunction],
      truthTables: List[(String, List[Int])]
  ): Unit =

    import NormalformUtils.*

    require(vars.nonEmpty, "Mindestens eine Variable erforderlich.")

    require(
      vars.distinct.length == vars.length,
      "Variablennamen muessen eindeutig sein."
    )

    require(
      vars.length < 31,
      "Zu viele Variablen fuer die Integer-Indizierung."
    )

    require(
      gateMode >= 0 && gateMode <= 2,
      "gateMode muss 0, 1 oder 2 sein."
    )

    require(
      inputMode == 0 || inputMode == 1,
      "inputMode muss 0 oder 1 sein."
    )

    val n = vars.length
    val combinations = 1 << n

    // Gleichungen oder Wahrheitstabellen verwenden

    val selectedFunctions: List[BoolFunction] =
      inputMode match

        case 0 => functions

        case 1 =>

          truthTables.map { case (name, values) =>

            require(
              values.length == combinations,
              s"$name muss genau $combinations Funktionswerte besitzen."
            )

            require(
              values.forall(v => v == 0 || v == 1),
              s"$name darf nur 0 oder 1 enthalten."
            )

            BoolFunction(
              name,
              assignment =>

                val index = vars.foldLeft(0) { (acc, v) =>
                  (acc << 1) |
                    (if assignment(v) then 1 else 0)
                }

                values(index) == 1
            )
          }

    // ==================================================
    // 7. ALLE FUNKTIONEN BERECHNEN
    // ==================================================

    selectedFunctions.foreach { function =>

      println("\n====================================")
      println("FUNKTION: " + function.name)
      println("====================================")

      // Wahrheitstabelle berechnen

      val rows = (0 until combinations).map { i =>

        val bits = binary(i, n)

        val assignment = vars
          .zip(
            bits.map(_ == '1')
          )
          .toMap

        (bits, function.eval(assignment))
      }

      println("\nWahrheitstabelle:")

      println(vars.mkString(" | ") + " | f")

      rows.foreach { case (bits, result) =>

        println(
          bits.mkString(" | ") +
            " | " + (if result then 1 else 0)
        )
      }

      // Minterme und Maxterme

      val ones = rows.zipWithIndex.collect { case ((_, true), index) =>
        index

      }.toSet

      val zeros = rows.zipWithIndex.collect { case ((_, false), index) =>
        index

      }.toSet

      // KDNF

      val kdnf =
        if ones.isEmpty then "0"
        else
          ones.toList.sorted
            .map { i =>
              minterm(vars, binary(i, n))
            }
            .mkString(" + ")

      // KKNF

      val kknf =
        if zeros.isEmpty then "1"
        else
          zeros.toList.sorted
            .map { i =>
              maxterm(vars, binary(i, n))
            }
            .mkString(" x ")

      println("\nMinterme:")
      println(ones.toList.sorted.mkString("m(", ", ", ")"))

      println("\nMaxterme:")
      println(zeros.toList.sorted.mkString("M(", ", ", ")"))

      println("\nKDNF:")
      println(kdnf)

      println("\nKKNF:")
      println(kknf)

      // Minimierte DNF

      val dnfPrimes = primeImplicants(ones, n)

      val dnfMinimum = minimumCover(
        dnfPrimes,
        ones
      )

      println("\nMinimierte DNF:")
      println(sop(vars, dnfMinimum))

      // Minimierte KNF

      val knfPrimes = primeImplicants(zeros, n)

      val knfMinimum = minimumCover(
        knfPrimes,
        zeros
      )

      println("\nMinimierte KNF:")
      println(pos(vars, knfMinimum))

      // ==================================================
      // 8. ZWEISTELLIGE GATTERDARSTELLUNG
      // ==================================================

      gateMode match

        case 0 =>
          ()

        case 1 =>

          println("\nZweistellige NAND-Darstellung:")

          val builder = new GateBuilder(1, vars)

          val result = builder.buildNandDNF(
            vars,
            dnfMinimum
          )

          builder.output(result)

        case 2 =>

          println("\nZweistellige NOR-Darstellung:")

          val builder = new GateBuilder(2, vars)

          val result = builder.buildNorKNF(
            vars,
            knfMinimum
          )

          builder.output(result)

      println("\n====================================")

    }
