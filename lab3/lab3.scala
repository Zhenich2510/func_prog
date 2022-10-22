object lab3{
  def printTask(number: Int): Unit = {
    println(s"\n\n-----------------Task $number--------------------")
  }

  def y(x: Double): Double = {
    3 * x - Math.cos(x)
  }

  def yf(x:Double):Double={
    Math.sin(x) + 3
  }

  def yff(x:Double): Double ={
    Math.cos(x)
  }

  def Dotychnih(x: Double, e: Double): Any = {
    if (Math.abs(x - y(x) / yf(x) - x) < e) {
      println("Результат - " + x)
    } else {
      println(x)
      Dotychnih(x - y(x) / yf(x), e)
    }
  }

  def lambd(x:Double):Double={
    1 / yf(x)
  }

  def iter(e:Double, a:Double):Any={
    def iteri(x0: Double, e: Double, xi: Double, oldx: Double): Any = {
      if (Math.abs(xi - oldx) <= e) {
        println("Результат - " + xi)
      } else {
        println(xi - (lambd(x0) * y(xi)))
        iteri(x0, e, xi - (lambd(x0) * y(xi)), xi)
      }
    }
    iteri(a, e, a, a+10)
  }

  def main1(e:Double, b:Double, a:Double):Any={
    println("Рішення через метод простої ітерації:")
    iter(e, a)
    println()
    println()

    println("Рішення через метод дотичних")
    if ( (y(a) * yff(a)) > 0){
      if (y(a) * yff(a) < Math.pow(yf(a), 2)){
        Dotychnih(a, e)
      }
      else println("Метод з таким значенням не збігаеться")
    }
    else println("Неможливо знайти рішення із таким початковим наближенням")
  }

//  task 2

  var res:Double = 0

  def y1(x:Double):Double={
    Math.pow(x, 4) * Math.pow(Math.E, Math.pow(-x, 2))
  }

  def iterrquadro(x1: Double, h: Double, n: Double): Double = {
    println(x1 + " - " + y1(x1 + h))
    res += y1(x1 + h)
    if (n > 0) {
      y1(x1 + h) + iterrquadro(x1 + h, h, n - 1)
    }else{
      println()
      println("Res - " + res)
      0
    }
  }

  def rquadro(a:Double, b:Double, n:Double):Double={
    println("Метод правих прямокутників")
    ((b - a) / n) * iterrquadro(a, (b-a)/n, n)
  }


//  2.2

  var res1: Double = 0

  def iterrtrapetcii(x1: Double, h: Double, n: Double): Double = {
    println(x1 + " - " + y1(x1 + h))
    res1 += y1(x1 + h)
    if (n > 0) {
      y1(x1 + h) + iterrtrapetcii(x1 + h, h, n - 1)
    } else {
      println()
      println("Res - " + (res1 + y1(x1 - h)))
      0
    }
  }

  def rtrapetcii(a: Double, b: Double, n: Double): Double = {
    println()
    println("Метод трапеції")
    ((b - a) / 2 * n) * iterrtrapetcii(a, (b - a) / 2 * n, n + 1)
  }



  def main(args: Array[String]): Unit = {
    printTask(1)
    main1(0.001, 5, 1)

    printTask(2)
    rquadro(0, 3.14, 1)
    rtrapetcii(0, 3.14, 1)

  }
}
