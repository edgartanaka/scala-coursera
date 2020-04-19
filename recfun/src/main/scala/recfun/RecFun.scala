package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == c || c == 0) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceRecur(chars, 0, 0)
  }

  def balanceRecur(chars: List[Char], open: Int, close: Int): Boolean = {
    if (chars.isEmpty)
      open == close
    else
      if (chars.head == '(')
        balanceRecur(chars.tail, open+1, close)
      else if(chars.head == ')')
        if(close + 1 > open) false else balanceRecur(chars.tail, open, close+1)
      else
        balanceRecur(chars.tail, open, close)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1 // we have found one valid combination of coins
    else if (coins.isEmpty || money < 0)
      0 // impossible to provide valid solution
    else {
      // variations with 0 coins of head + variations with 1 coin of head
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
