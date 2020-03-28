package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if r == 0 || c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def withStack(chars: List[Char], stack: List[Char]): Boolean =
      if chars.isEmpty then stack.isEmpty
      else if chars.head == ')' then
        if stack.isEmpty then false
        else withStack(chars.tail, stack.tail)
      else if chars.head == '(' then 
        withStack(chars.tail, stack :+ chars.head)
      else withStack(chars.tail, stack)
    
    val stack = List()
    withStack(chars, stack) 

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then 1 // change done
    else if money < 0 || coins.isEmpty then 0 // impossible change
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    
}
