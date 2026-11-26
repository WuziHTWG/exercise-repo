package functions:
    import classes.*
    import java.time.LocalDate
    import scala.io.Source
    import scala.util.Try

    def validMatrikel(s: String): Option[String] =
        if s.matches("[0-9]+") then scala.util.Try(s).toOption
        else None

    def validEmail(s: String): Option[String] =
        if s.contains("@") then Some(s)
        else None

    def validDate(s: String): Option[LocalDate] =
        scala.util.Try(LocalDate.parse(s)).toOption

    def toStudent(in: StudentInput): Option[Student] =
        for
            m <- validMatrikel(in.matrikel)
            e <- validEmail(in.email)
            d <- validDate(in.enrolled)
        yield Student(m, e, d)

    def readConfig(path: String): Try[List[String]] =
        Try {
            val source = Source.fromFile(path)
            try source.getLines().toList
            finally source.close()
        }

    def parseLine(line: String): Try[StudentInput] =
        Try {
            val fields = line.split(",", -1)

            require(fields.length == 3, "Line must have exactly 3 fields")

            StudentInput(
              fields(0).trim,
              fields(1).trim,
              fields(2).trim
            )
        }
