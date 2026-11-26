import com.typesafe.scalalogging.LazyLogging
import scala.util.{Try, Success, Failure}
import functions.*

object StudentApp extends LazyLogging:
    def run(): Unit =
        readConfig("scalasource/aufg15/src/students.csv") match

            case Success(lines) =>

                lines.foreach { line =>
                    logger.debug(s"Processing line: $line")
                    parseLine(line) match

                        case Success(input) =>

                            toStudent(input) match

                                case Some(student) =>
                                    println(s"VALID STUDENT: $student")

                                case None =>
                                    logger.warn(s"Invalid student input: $input")
                                    println(s"INVALID INPUT: $input")

                        case Failure(exception) =>
                            logger.error(s"Failed to parse line: $line"/*, exception*/)
                            println(s"FAILED LINE: $line")
                            println(s"ERROR: ${exception.getMessage}")
                }

            case Failure(exception) =>
                logger.error("Failed to read students.csv", exception)
                println(s"FILE ERROR: ${exception.getMessage}")

@main def main(): Unit =
    StudentApp.run()
