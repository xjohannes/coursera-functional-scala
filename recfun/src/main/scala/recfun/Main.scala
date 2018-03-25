package recfun

object Main {
  def main(args: Array[String]) {
    //countChange(4, List(1, 2))
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def change(amount: Int, coins: List[Int], n: Int): Int =
      if(coins.isEmpty) 0 else {
        if(amount <= 0) {
          if(amount == 0) {
            if(n == 0) 1 + change(money, checkN(coins.tail), newN(money, coins.tail), 1) else 1 + change(money, coins, n -1)
          } else 0 + change(money, checkN(coins), newN(money, coins.tail))
        } else {
          if(n == 0) change(amount, coins.tail, newN(amount, coins.tail)) else {
            changeMoney(amount - (coins.head * n), coins, newN(amount - (coins.head * n), coins.tail), 1) //toDo 1 == currentListPointer, how to calculate it
          }
        }
    }
    def checkN(coinsTail:List[Int]):List[Int] =
      if(coinsTail.isEmpty) Nil else coinsTail

    def newN(money: Int, coins: List[Int]): Int =
      if(money <= 0 || coins.isEmpty || coins.head == 0) 0
      else (money/ coins.head).floor.toInt
    def changeMoney(amount: Int, coins: List[Int], n: Int, currentListPointer: Int): Int = ???

    if(money != 0) change(money, coins.sorted.reverse, newN(money, coins.sorted.reverse)) else 0
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = if (c == 0) 1 else (r - c) + 1


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val parenteseiesOnly = chars.filter(c => c == '(' || c == ')')

    def matched(current: List[Char], returned: List[Char]): List[Char] = {
      if (current.isEmpty && returned.isEmpty) {
        Nil
      } else {
        if (returned.isEmpty) {
          current.head match {
            case '(' => throw new Exception(s"Current: '(', did not match returned Nil")
            case ')' => List(')')
            case _ => Nil
          }
        } else {
          if (current.isEmpty) {
            returned.head match {
              case '(' => throw new Exception(s"Current: '(', did not match returned '('")
              case ')' => returned
              case _ => Nil
            }
          } else {
            if (returned == '(') {
              throw new Exception(s"Returned was '('")
            } else {
              if (current.head == '(' && returned.head == ')') {
                Nil
              } else {
                if (current.head == ')' && returned.head == ')') {
                  ')' :: returned
                } else {
                  throw new Exception("General error")
                }
              }
            }
          }
        }

      }
    }

    def inner(chars: List[Char]): List[Char] = {
      if (chars.isEmpty) {
        Nil
      } else {
        if (chars.head == '(' && chars.tail.nonEmpty) {
          if (chars.tail.head == ')') {
            if (chars.tail.tail.nonEmpty) {
              matched(Nil, inner(chars.tail.tail))
            } else {
              Nil
            }
          } else {
            matched(List(chars.head), inner(chars.tail))
          }
        } else { // ')'
          if (chars.tail.isEmpty) {
            matched(List(chars.head), inner(chars.tail))
          } else {
            chars
          }
        }
      }
    }

    if (parenteseiesOnly.nonEmpty && parenteseiesOnly.head == '(') {
      try {
        if (inner(parenteseiesOnly).isEmpty) true
        else false
      } catch {
        case e: Exception => println(s"Caught error $e")
          false
      }
    } else false
  }
}
