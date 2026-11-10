@main def hospital(): Unit =
  object Utils:
    def test(): Unit = ???
    def numberEnding(n: Int): String = (n % 10) match
      case 1 => "st"
      case 2 => "nd"
      case 3 => "rd"
      case _ => "th"

  enum roomType():
    case icu
    case regular
    case operatingRoom
    case deliveryRoom
    case treatmentRoom

  enum treatmentType():
    case cSection
    case intubation
    case amputation

  enum Department:
    case neurology
    case physio
    case cardiology

  enum availability():
    case available
    case scarceAvailable
    case justFewLeft
    case inavailable

  enum restriction():
    case unrestricted
    case lightlyRestricted
    case restricted
    case majorlyRestricted

  import Utils.* // import of the Utils, needed in the classes
  import roomType.* // import of the Enums for easier use
  import treatmentType.*
  import Department.*
  import availability.*
  import restriction.*

  class Patient(firstName: String, lastName: String):
    private var _name = (firstName, lastName)
    def name: String = _name.toString()
    def setName(s: String): Unit = _name = (s.split(' ').toList(0), s.split(' ').toList(1))

  class Room(val rType: roomType, val roomNumber: String):
    private var _occupied: Boolean = false
    private var _runningnumber = Room.runNumGenerator()
    def occupied: Boolean = _occupied
    def setStatus(change: Boolean): Unit = _occupied = change
    override def toString(): String = s"Room $roomNumber is a $rType Type and is ${
        if _occupied == true then "occupied" else "unoccupied"
      } it is the $_runningnumber${numberEnding(_runningnumber)} Room in the hospital"
  object Room:
    private var count = 0
    def runNumGenerator(): Int =
      count += 1
      count

  class Personal(firstName: String, lastName: String):
    private var _name = (firstName, lastName)
    def name: String = _name.toString()
    def setName(s: String): Unit = _name = (s.split(' ').toList(0), s.split(' ').toList(1))

  case class Operations()
  object Operations // companion-object for operations

  class OngoingTreatment(
      val staff: Personal,
      val patient: Patient,
      val Room: Room,
      val treatment: treatmentType,
      val Department: Department
  )
  object OngoingTreatment:
    def price(): Double = ???

  case class medication(
      var ttype: treatmentType,
      val dept: Department,
      val res: restriction,
      val name: String,
      var stock: Int
  ):
    var ava: availability = available
    def used(i: Int): Int = stock - i
    def stocked(i: Int): Int = stock + i

  println(Room(regular, "O001"))
  println(Room(regular, "O002"))
  println(Room(regular, "O003"))
  println(Room(regular, "O004"))
  println(Room(regular, "O005"))
  println(Room(regular, "O006"))
  println(Room(icu, "I01"))
  val r = Room(icu, "O020")
  r.setStatus(true)
  println(r.occupied)
