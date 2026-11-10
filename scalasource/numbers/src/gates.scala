import scala.compiletime.ops.boolean
def gates(): Unit =
  object gatesUtils:
    def and(a: Boolean, b: Boolean): Boolean = (a, b) match
      case (true, true) => true
      case _            => false

    def or(a: Boolean, b: Boolean): Boolean = (a, b) match
      case (false, false) => false
      case _              => true
    def not(a: Boolean): Boolean =
      if a then false else true
    def xor(a: Boolean, b: Boolean): Boolean = or(and(not(a), b), and(a, not(b)))
    def xnor(a: Boolean, b: Boolean): Boolean = not(xor(a, b))
    def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
    def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
