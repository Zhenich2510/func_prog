import scala.io.StdIn

object lab1 {

  // Вивод номера завдання
  def printTask(number: Int): Unit = {
    println(s"\n\n-----------------Task $number--------------------")
  }

  var countOfRecursive: Int = 0 // Створюю змінну що рахує кількість рекурсії

  def P(n: Int, m: Int): Int = { // Функція що рахує кількість розбиття (По формулі)
    countOfRecursive += 1
    if (m == 0 || n < 0) {
      0
    } else if (n == 0 && m == 0) {
      1
    } else if (n == m){
      1 + P(n, m-1)
    }
    else {
      P(n, m - 1 + P(n - m, m))
    }
  }

  // Найбільший спільний дільник
  def gcd(m: Int, n: Int): Int = {
    if (n == 0) {m}
    else gcd(n, m % n)
  }

  // Найменше спільне кратне
  def lcm(m: Int, n: Int): Int = {
    (m * n).abs / gcd(m, n)
  }

  // Основна функція для виводу
  def main(args: Array[String]): Unit ={

    printTask(1)
    println("Type something : ")
    val n = StdIn.readLine("N is ")
    val m = StdIn.readLine("M is ")

    println(P(n.toInt, m.toInt))
    println(s"Count number of recursive calls $countOfRecursive")


    printTask(2)

    val n1 = StdIn.readLine("First digit: ")
    val n2 = StdIn.readLine("Second digit:  ")
    println(lcm(n1.toInt, n2.toInt))
  }
}
