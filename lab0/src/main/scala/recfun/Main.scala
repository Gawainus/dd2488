package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    val arr1 = Array[1, 2]
    var arr2 = new Array[Int](3)

    val head = arr1.head

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    return 0
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    return true
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    return 0
  }

}
