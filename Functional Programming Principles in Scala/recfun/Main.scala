package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c < 0 || r < 0 || c > r) 0
    else if (c == 0) 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = chars.foldLeft(0)((x: Int, y: Char) => if (y == ')' && x == 0) -100000 else if (y == ')') x - 1 else if (y == '(') x + 1 else x) >= 0

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)

}