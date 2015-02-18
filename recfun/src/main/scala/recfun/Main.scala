import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println(countChange(4, List(2, 1)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = c match {
    case 0 => 1
    case `r` => 1
    case _ => pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def balance(chars: List[Char], weight: Int): Boolean = {
      if (weight < 0) false else
      if (chars.isEmpty) weight == 0 else
      balance(chars.tail, weight + (chars.head match {
        case '(' => 1
        case ')' => -1
        case _ => 0        
      }))
    }
    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      // three cases are:
      //
      //  1. whatever we've already handed over is the correct amount of money
      //  2. if there's still money owed, either continue adding instances of the current denomination or don't
      //  3. otherwise we don't need to hand over more money and haven't hit the right total, so we've pursued an invalid combination
      //
      if(money == 0) 1 else
      if(money > 0 && !coins.isEmpty) countChange(money - coins.head, coins) + countChange(money, coins.tail) else
      0
  }
}
