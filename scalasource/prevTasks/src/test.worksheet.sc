/*println("helloWorld")
val x = 20
println(x)
val y = 30
val z = y % x
println(z)
println(y)
println("This is a test")*/

val c = 51966.toBinaryString
val d = 51966.toHexString
println(c)
println(d)
println(Int.MaxValue + 1)
println(0.1 + 0.2)
var a: BigInt = 1
for i <- 1 to 25 do a = a * i
println(a)
println((1 to 20 by 4).toList)
println((1 to 20 by 4).toList.map(x => x * 2))

val array = Array(1, 2, 4, 5)
val squaredArray = for (elem <- array) yield math.pow(elem, 2)
println(squaredArray)
println(squaredArray.toList)
def safeDiv(a: Int, b: Int): Either[String, Int] =
 if b == 0 then Left("division by zero")
 else Right(a / b)
safeDiv(7, 2).getOrElse(0)

def parseAge(s: String): Either[String, Int] =
	s.toIntOption match
	case None => Left("not a number")
	case Some(n) if n < 0 => Left("negative")
	case Some(n) if n > 150 => Left("too old")
	case _ => Right(s.toInt)

parseAge("160")
parseAge("30")
parseAge("forty")
parseAge("-3")

import scala.util.Try
val xs = List("10", "20", "abc", "5", "hello", "15")
def sumValid(xs: List[String]): Int = 
  val numbers = for 
    s <- xs
    n <- Try(s.toInt).toOption
  yield n
    numbers.sum

sumValid(xs)
val numbers = for {
    s <- xs
    n <- Try(s.toInt).toOption
  } yield n

Try("1 2".toInt).getOrElse(-1)
import scala.io.{Source, BufferedSource}
import scala.util.{Try, Success, Failure}
def countLines(filename: String): Try[Int] =
  var source: BufferedSource = null
  try
   source = Source.fromFile(filename)
   Success(source.getLines.length)
  catch
   case t: Throwable => Failure(t)
  finally
  if source != null then
   source.close()
val f = java.io.File.createTempFile("lines", ".txt")
java.nio.file.Files.writeString(f.toPath, "a\nb\nc")

countLines(f.getPath())