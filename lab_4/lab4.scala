import scala.io.StdIn
import scala.util.Random
import util.Random.nextInt

object lab4 {

  def printTask(number: Int): Unit = {
    println(s"\n\n-----------------Task $number--------------------")
  }

  def negative_numbers_squared(list: List[Int], idx: Int): List[Int] = {
    var new_list = list
    if (idx >= new_list.length) {
      return new_list
    }
    else if (new_list(idx) < 0) {
      new_list = new_list.updated(idx , new_list(idx) * new_list(idx))
    }

    new_list = negative_numbers_squared(new_list,  idx+1)
    new_list
  }


  def sum_of_even_numbers(list: List[Int], idx: Int): Int = {
    var sum = 0
    if (idx >= list.length) {
      return sum
    }
    else if (list(idx) % 2 == 0) {
      sum = list(idx)
    }
    sum += sum_of_even_numbers(list, idx + 1)

    sum
  }


  def getLargest[T <% Ordered[T]](data: List[T]): (T, List[T]) =
    data match {
      case head :: Nil => (head, Nil)
      case head :: tail =>
        val (large, remaining) = getLargest(tail)
        if (large < head)
          (large, head :: remaining)
        else
          (head, large :: remaining)
    }

  def bubbleSort[T <% Ordered[T]](data: List[T]): List[T] =
    data match {
      case Nil => Nil
      case _ =>
        val (greatest, tail) = getLargest(data)
        bubbleSort(tail) ::: List(greatest)
    }

  def main(args: Array[String]): Unit = {
    printTask(1)

    var len_list = StdIn.readLine("How long list: ")
    var list_1 = Seq.fill(len_list.toInt)(Random.between(-10, 10)).toList
    println(list_1)

    var q1 = negative_numbers_squared(list_1, 0)
    var q2 = bubbleSort(list_1)
    var q3 = sum_of_even_numbers(list_1, 0)

    println(s"Negative numbers squared: $q1")
    println(s"Sorted list: $q2")
    println(s"Sum of all even digits: $q3")

    printTask(2)


  }
}
