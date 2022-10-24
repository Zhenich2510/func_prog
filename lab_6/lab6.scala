import scala.collection.mutable._

case object lab6{

  def printTask(number: Int): Unit = {
    println(s"\n\n-----------------Task $number--------------------")
  }

  val vec1 :Vector[Int] = Vector(0, 3, 4, 5, 7, 8, 45, 787, 45, -34, 56)
  val vec2 :Vector[Int] = Vector(4, 6, 77, 34, 56, 7, 22, -45, 56, 76, 111)

  var list1 :List[Int] = List()


  // Найбільший спільний дільник
  def gcd(m: Int, n: Int): Int = {
    if (n == 0) {
      m
    }
    else gcd(n, m % n)
  }

  // Найменше спільне кратне
  def lcm(m: Int, n: Int): Int = {
    ((m * n).abs / gcd(m, n)).abs
  }

  def vector_with_LCM(v1: Vector[Int], v2: Vector[Int], i: Int): Unit = {
    var min_len = 0
    if (vec1.length < vec2.length){
      min_len = vec1.length
    }else{
      min_len = vec2.length
    }

    if (i == min_len){
      return 0
    }

    list1 = list1 ::: List(lcm(v1(i), v2(i)))

    vector_with_LCM(v1, v2, i+1)
  }

// task 2
  var q1 = Queue("One", "OOP", "KISS", "DRY", "Engeneer")

  var s = Stack[String]()

  var q2 = Queue[String]()

  def queue_to_stack(q:Queue[String], s:Stack[String], len:Int, i:Int):Unit={
    if (len > q.length){
      println("Занадто багато сиволів!")
    }else{
      if (i == len){
        return 0
      }
      s.push(q(i))

      queue_to_stack(q, s, len, i+1)
    }

  }

  def reverse_stack_to_reverse_queue(s:Stack[String], q:Queue[String], q2:Queue[String],  i:Int): Unit={
    var ii = 0

    if (i < q.length && q2.length <= q.length){

      q2 += q(i)

      reverse_stack_to_reverse_queue(s, q, q2, i + 1)

    }else{

      if (s.isEmpty){
        return q2.reverse
      }

      q2 += s.pop

      reverse_stack_to_reverse_queue(s, q, q2, ii + 1)

      return 0
    }
  }


  def main(args: Array[String]): Unit = {
    printTask(1)

    vector_with_LCM(vec1, vec2, 0)
    val vec3 = list1.toVector
    println(vec3)

    printTask(2)

    println(q1)

    queue_to_stack(q1, s, 3, 0)
    println(s)

    reverse_stack_to_reverse_queue(s, q1, q2, 0)
    println(q2.reverse)


  }
}
