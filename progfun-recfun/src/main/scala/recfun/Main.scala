package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))

    println(countChange(4, List(1, 2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def count(list: List[Char]): Int = {
      if (list.isEmpty)
        0
      else if (list.head.==('('))
        1 + count(list.tail)
      else if (list.head == (')'))
        checkClose(-1 + count(list.tail))
      else
        count(list.tail)
    }

    def checkClose(closeCounts: Int): Int = {
      if (closeCounts < 0)
        -2
      else
        closeCounts
    }

    count(chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
