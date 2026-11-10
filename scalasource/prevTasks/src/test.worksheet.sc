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
