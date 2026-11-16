import scala.collection.View.Empty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
var name: List[Int] = List(1, 3, 5, 6)
val t = name.map
val string = "test String"
string.split(' ').toList
val args = Seq("Ada", "3")
string(0)
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
def history(steps: List[Int]): ArrayBuffer[Int] =
  var acc = ArrayBuffer[Int]()
  steps.foreach { x =>
    acc += x + (if !acc.isEmpty then acc.last else 0)
  }
  acc
history(List(5, -2, 10)) 

def removeAll(ab: ArrayBuffer[Int], x: Int): Int =
  var count = 0
  var i = 0 
  while i < ab.length do
    if ab(i) == x then
      ab.remove(i)
      count += 1
    else i += 1
  count
val a = ArrayBuffer(2, 4, 4, 4, 5, 6)
removeAll(a, 4)
/*def index(words: List[String]): mutable.Map[Char, List[String]] =
	var map = mutable.Map[Char, List[String]]()
	for w <- words do map(w(0)) = map.getOrElse(w(0), List()) :+ w
*/

/*def removeExpired(items: mutable.Map[String, Int], today: Int): List[String] =
	//for i <- items if items(i) >= today yield List(i)
	var list = List[String]()
	for i <- items.keys if (items(i) < today) do list +: i
  items --= list
	list.sorted
val tMap = mutable.Map("milk" -> 3, "eggs" -> 5, "bread" -> 10, "cheese" ->2)
removeExpired(tMap, 5) */
val m = Map(1 -> "a", 2 -> "bb")
val lengths = for (k, v)<- m yield k * v.length

//Cursed as Fuck
def alphabetIndex(): Map[Char, Int] =
	('a' to 'z').toList.zip((1 to 26).toList).toMap
