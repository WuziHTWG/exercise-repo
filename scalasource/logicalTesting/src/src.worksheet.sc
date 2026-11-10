import scala.collection.View.Empty
var name: List[Int] = List(1, 3, 5, 6)
val t = name.map
val string = "test String"
string.split(' ').toList
val args = Seq("Ada", "3")

def script(args: Seq[String]): String =
  if args.length < 2 then "usage: greet <name> <times>"
  else ("Hi " + args(0) + "! ") * args(1).toInt
script(args)
string.toString()
name.toList

val ti = 2123

def rec(x: Int): Int =
  if x != 0 then (x % 10) + rec(x / 10) else x
rec(ti)

def reverse(remaining: Long, acc: Long): Long =
  if remaining == 0 then acc
  else reverse(remaining / 10, acc * 10 + remaining % 10)

reverse(ti, 0)

val s = Array('h', 'e', 'l', 'l', 'o')

/*def reverseString(s: Array[Char]): Unit =
  var n = 0
  def reverse(n: Int, s: Array[Char]): Array[Char] =
    if n == (s.length / 2) then s
    else
      val temp = s(n)
      s(n) = s(s.length - 1 - n)
      s(s.length - 1 - n) = temp
      reverse(n + 1, s)
    reverse(n, s)
reverseString(s)*/

s.foldLeft(List[Char]())((List, elem) => elem +: List)
def runningSum(nums: Array[Int]): Array[Int] =
  nums.foldLeft(Array.empty[Int])((acc, x) => acc :+ (acc.sum + x))
val i = Array(1, 2, 3, 4)
runningSum(i)
