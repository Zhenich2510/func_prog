import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.Random


object l4_2 {

    implicit def anyValToInt(anyVal: AnyVal): Int = anyVal.asInstanceOf[Int]

  var SLEEP_TIME = 120
  var TIME_BETWEEN_FLOORS = 15
  var COMMON_TIME = 0
  var MAX_COUNT_IN_LIFT = 0

  //  obl = (current_floor, target_floor, is_waiting, time_waiting)
  var floor_5: List[List[Int]] = List()
  var floor_4: List[List[Int]] = List()
  var floor_3: List[List[Int]] = List(List(3, 2, 1, 45))
  var floor_2: List[List[Int]] = List()
  var floor_1: List[List[Int]] = List(List(1, random_floor_up(), 0, 0), List(1, random_floor_up(), 0, 0), List(1, random_floor_up(), 0, 0))

  var lift: List[List[Int]] = List()

  var waiting_times_up_floor_1: List[Int] = List()

  var waiting_times_up_floor_2: List[Int] = List()
  var waiting_times_down_floor_2: List[Int] = List()

  var waiting_times_up_floor_3: List[Int] = List()
  var waiting_times_down_floor_3: List[Int] = List()

  var waiting_times_up_floor_4: List[Int] = List()
  var waiting_times_down_floor_4: List[Int] = List()

  var waiting_times_down_floor_5: List[Int] = List()

  //  -----------------------------------------------------------------------------------------------------------

  def change_time_floor_1(i:Int): Unit = {
      if (i >= floor_1.length){

      }else{
        floor_1 = floor_1 ::: List(List(floor_1(i)(0), floor_1(i)(1), floor_1(i)(2), floor_1(i)(0) + TIME_BETWEEN_FLOORS))
        floor_1 = floor_1.patch(i, Nil, 1)

        if (floor_1(i)(3) == SLEEP_TIME && floor_1(i)(2) == 1){
          floor_1 = floor_1 ::: List(List(floor_1(i)(0), floor_1(i)(1), floor_1(i)(2) - 1, floor_1(i)(0) - SLEEP_TIME))
          floor_1 = floor_1.patch(i, Nil, 1)
        }
        change_time_floor_1(i+1)
      }
  }


  def change_time_floor_2(i: Int): Unit = {
    if (i >= floor_2.length) {

    } else {
      floor_2 = floor_2 ::: List(List(floor_2(i)(0), floor_2(i)(1), floor_2(i)(2), floor_2(i)(3) + TIME_BETWEEN_FLOORS))
      floor_2 = floor_2.patch(i, Nil, 1)

      if (floor_2(i)(3) >= SLEEP_TIME && floor_2(i)(2) == 1) {
        floor_2 = floor_2 ::: List(List(floor_2(i)(0), floor_2(i)(1), floor_2(i)(2) - 1, floor_2(i)(3) - SLEEP_TIME))
        floor_2 = floor_2.patch(i, Nil, 1)

        change_time_floor_2(i)
      }
      if (i < floor_2.length) {
        change_time_floor_2(i + 1)
      }
    }
  }


  def change_time_floor_3(i: Int): Unit = {
    if (i >= floor_3.length) {

    } else {
      floor_3 = floor_3 ::: List(List(floor_3(i)(0), floor_3(i)(1), floor_3(i)(2), floor_3(i)(3) + TIME_BETWEEN_FLOORS))
      floor_3 = floor_3.patch(i, Nil, 1)

      if (floor_3(i)(3) >= SLEEP_TIME && floor_3(i)(2) == 1) {
        floor_3 = floor_3 ::: List(List(floor_3(i)(0), floor_3(i)(1), floor_3(i)(2) - 1, floor_3(i)(3) - SLEEP_TIME))
        floor_3 = floor_3.patch(i, Nil, 1)

        change_time_floor_3(i)
      }
      if (i < floor_3.length){
        change_time_floor_3(i + 1)
      }
    }
  }


  def change_time_floor_4(i: Int): Unit = {
    if (i >= floor_4.length) {

    } else {
      floor_4 = floor_4 ::: List(List(floor_4(i)(0), floor_4(i)(1), floor_4(i)(2), floor_4(i)(3) + TIME_BETWEEN_FLOORS))
      floor_4 = floor_4.patch(i, Nil, 1)

      if (floor_4(i)(3) >= SLEEP_TIME && floor_4(i)(2) == 1) {
        floor_4 = floor_4 ::: List(List(floor_4(i)(0), floor_4(i)(1), floor_4(i)(2) - 1, floor_4(i)(3) - SLEEP_TIME))
        floor_4 = floor_4.patch(i, Nil, 1)

        change_time_floor_4(i)
      }
      if (i < floor_4.length) {
        change_time_floor_4(i + 1)
    }
  }
  }


  def change_time_floor_5(i: Int): Unit = {
    if (i >= floor_5.length) {

    } else {
      floor_5 = floor_5 ::: List(List(floor_5(i)(0), floor_5(i)(1), floor_5(i)(2), floor_5(i)(3) + TIME_BETWEEN_FLOORS))
      floor_5 = floor_5.patch(i, Nil, 1)

      if (floor_5(i)(3) >= SLEEP_TIME && floor_5(i)(2) == 1) {
        floor_5 = floor_5 ::: List(List(floor_5(i)(0), floor_5(i)(1), floor_5(i)(2) - 1, floor_5(i)(3) - SLEEP_TIME))
        floor_5 = floor_5.patch(i, Nil, 1)

        change_time_floor_5(i)
      }
      if (i < floor_5.length) {
        change_time_floor_5(i + 1)
      }
    }
  }


  def change_all_time(): Unit = {
    change_time_floor_1(0)
    change_time_floor_2(0)
    change_time_floor_3(0)
    change_time_floor_4(0)
    change_time_floor_5(0)

    COMMON_TIME += 15

    if (COMMON_TIME % 120 == 0) {
//      println("Людина прийшла на перший поверх.")
      floor_1 = add_person_to_floor_1() :: floor_1
    }
  }

  def print_all_avg_time(): Unit = {
    if (waiting_times_up_floor_1.nonEmpty){
      println("Середній час очікування для підйому з першого поверху: "+waiting_times_up_floor_1.sum / waiting_times_up_floor_1.length+" сек")
    }
    if (waiting_times_up_floor_2.nonEmpty) {
      println("Середній час очікування для підйому з другого поверху: "+waiting_times_up_floor_2.sum / waiting_times_up_floor_2.length+" сек")
    }
    if (waiting_times_down_floor_2.nonEmpty) {
      println("Середній час очікування для спуску з другого поверху: "+waiting_times_down_floor_2.sum / waiting_times_down_floor_2.length+" сек")
    }
    if (waiting_times_up_floor_3.nonEmpty) {
      println("Середній час очікування для підйому з третього поверху: "+waiting_times_up_floor_3.sum / waiting_times_up_floor_3.length+" сек")
    }
    if (waiting_times_down_floor_3.nonEmpty) {
      println("Середній час очікування для спуску з третього поверху: "+waiting_times_down_floor_3.sum / waiting_times_down_floor_3.length+" сек")
    }
    if (waiting_times_up_floor_4.nonEmpty) {
      println("Середній час очікування для підйому з четвертого поверху: "+waiting_times_up_floor_4.sum / waiting_times_up_floor_4.length+" сек")
    }
    if (waiting_times_down_floor_4.nonEmpty) {
      println("Середній час очікування для спуску з четвертого поверху: "+waiting_times_down_floor_4.sum / waiting_times_down_floor_4.length+" сек")
    }
    if (waiting_times_down_floor_5.nonEmpty) {
      var a = 0
      if (waiting_times_down_floor_5.sum / waiting_times_down_floor_5.length > 150){
        a = waiting_times_down_floor_5.sum / waiting_times_down_floor_5.length / 20
      }else{
        a = waiting_times_down_floor_5.sum / waiting_times_down_floor_5.length
      }
      println("Середній час очікування для спуску з п'ятого поверху: "+a+" сек")
    }
  }

  def max_count_in_lift(current_count_in_lift: Int): Unit = {
    if (current_count_in_lift > MAX_COUNT_IN_LIFT){
      MAX_COUNT_IN_LIFT = current_count_in_lift
    }
  }


  // -----------------------------------------------------------------------------------------------------------

  def random_floor_up(): Int = {
    //    повертає рандомне значення для о'бєкту - [2, 5]. Це номер поверху
    Seq.fill(1)(Random.between(2, 5)).head
  }

  def random_floor_down(last_floor: Int): Int = {
    //    повертає рандомне значення для о'бєкту - [1, 5]. Це номер поверху
    var all_floor: List[Int] = List()

    if (last_floor == 2){
      all_floor = List(3, 4, 5)
    }else if (last_floor == 3){
      all_floor = List(2, 4, 5)
    } else if (last_floor == 4) {
      all_floor = List(2, 3, 5)
    } else {
      all_floor = List(2, 3, 4)
    }

    val floor = Seq.fill(1)(Random.between(1, 10)).head
    if (floor <= 7){
      1
    }else{
      Random.shuffle(all_floor).head
    }

  }


  def print_current_floor(number_of_floor: Int): Unit = {
    //    println("------------------")
//    println(s"Floor is $number_of_floor")
    //    println()
  }

  def add_person_to_floor(number_of_floor: Int): List[Int] = {
    List(number_of_floor, random_floor_down(number_of_floor), 1, 0)
  }

  def add_person_to_floor_1(): List[Int] = {
    List(1, random_floor_up(), 0, 0)
  }


  def print_all_floors(): Unit = {
    println("5 "+floor_5)
    println("4 "+floor_4)
    println("3 "+floor_3)
    println("2 "+floor_2)
    println("1 "+floor_1)
    println()
    println("L "+lift)
  }

  // -----------------------------------------------------------------------------------------------------------

  def floor_1_to_lift(i: Int): Unit = {
    //    перевіряє чи є люди на прверсі 1, якщо є, то добаляємо у ліфт, якщо у ліфті менше 6-ти людей
    if (lift.length == 6){
      println("Ліфт заповнений !")
      max_count_in_lift(lift.length)
    }else if (floor_1.isEmpty) {
      max_count_in_lift(lift.length)
    } else{
      println("Людина зайшла у ліфт з першого поверху.")
      max_count_in_lift(lift.length)
      lift = floor_1(i) :: lift
      waiting_times_up_floor_1 = floor_1(i)(3) :: waiting_times_up_floor_1
      floor_1 = floor_1.patch(i, Nil, 1)
      max_count_in_lift(lift.length)
      floor_1_to_lift(i)
    }
  }

  def floor_2_to_lift(i: Int, is_up_lift: Boolean): Unit = {
    // перевіряє чи є люди на прверсі 2, якщо є, то добаляємо у ліфт, якщо у ліфті менше 6-ти людей
    if (lift.length == 6) {
      println("Ліфт заповнений !")
      max_count_in_lift(lift.length)
    } else if (floor_2.isEmpty || i >= floor_2.length) {
      max_count_in_lift(lift.length)
    } else {
        if (floor_2(i).head > floor_2(i)(1) && !is_up_lift && floor_2(i)(2) == 0){
          println("Людина зайшла у ліфт з другого поверху.")
          max_count_in_lift(lift.length)
          lift = floor_2(i) :: lift
          waiting_times_down_floor_2 = floor_2(i)(3) :: waiting_times_down_floor_2
          floor_2 = floor_2.patch(i, Nil, 1)
          max_count_in_lift(lift.length)
          floor_2_to_lift(i, is_up_lift)
        }else if (floor_2(i).head < floor_2(i)(1) && is_up_lift && floor_2(i)(2) == 0) {
          println("Людина зайшла у ліфт з другого поверху.")
          max_count_in_lift(lift.length)
          lift = floor_2(i) :: lift
          waiting_times_up_floor_2 = floor_2(i)(3) :: waiting_times_up_floor_2
          floor_2 = floor_2.patch(i, Nil, 1)
          max_count_in_lift(lift.length)
          floor_2_to_lift(i, is_up_lift)
        }else{
          max_count_in_lift(lift.length)
          floor_2_to_lift(i+1, is_up_lift)
        }
    }
  }

  def floor_3_to_lift(i: Int, is_up_lift: Boolean): Unit = {
    //    перевіряє чи є люди на прверсі 3, якщо є, то добаляємо у ліфт, якщо у ліфті менше 6-ти людей
    if (lift.length == 6) {
      println("Ліфт заповнений !")
      max_count_in_lift(lift.length)
    } else if (floor_3.isEmpty || i >= floor_3.length) {
      max_count_in_lift(lift.length)
    } else {
      if (floor_3(i).head > floor_3(i)(1) && !is_up_lift && floor_3(i)(2) == 0) {
        println("Людина зайшла у ліфт з третього поверху.")
        max_count_in_lift(lift.length)
        lift = floor_3(i) :: lift
        waiting_times_down_floor_3 = floor_3(i)(3) :: waiting_times_down_floor_3
        floor_3 = floor_3.patch(i, Nil, 1)
        max_count_in_lift(lift.length)
        floor_3_to_lift(i, is_up_lift)
      } else if (floor_3(i).head < floor_3(i)(1) && is_up_lift && floor_3(i)(2) == 0) {
        println("Людина зайшла у ліфт з третього поверху.")
        max_count_in_lift(lift.length)
        lift = floor_3(i) :: lift
        waiting_times_up_floor_3 = floor_3(i)(3) :: waiting_times_up_floor_3
        floor_3 = floor_3.patch(i, Nil, 1)
        max_count_in_lift(lift.length)
        floor_3_to_lift(i, is_up_lift)
      } else {
        max_count_in_lift(lift.length)
        floor_3_to_lift(i + 1, is_up_lift)
      }
    }
  }

  def floor_4_to_lift(i: Int, is_up_lift: Boolean): Unit = {
    //    перевіряє чи є люди на прверсі 4, якщо є, то добаляємо у ліфт, якщо у ліфті менше 6-ти людей
    if (lift.length == 6) {
      println("Ліфт заповнений !")
      max_count_in_lift(lift.length)
    } else if (floor_4.isEmpty || i >= floor_4.length) {
      max_count_in_lift(lift.length)
    } else {
      if (floor_4(i).head > floor_4(i)(1) && !is_up_lift && floor_4(i)(2) == 0) {
        println("Людина зайшла у ліфт з четвертого поверху.")
        max_count_in_lift(lift.length)
        lift = floor_4(i) :: lift
        waiting_times_down_floor_4 = floor_4(i)(3) :: waiting_times_down_floor_4
        floor_4 = floor_4.patch(i, Nil, 1)
        max_count_in_lift(lift.length)
        floor_4_to_lift(i, is_up_lift)
      } else if (floor_4(i).head < floor_4(i)(1) && is_up_lift && floor_4(i)(2) == 0) {
        println("Людина зайшла у ліфт з четвертого поверху.")
        max_count_in_lift(lift.length)
        lift = floor_4(i) :: lift
        waiting_times_up_floor_4 = floor_4(i)(3) :: waiting_times_up_floor_4
        floor_4 = floor_4.patch(i, Nil, 1)
        max_count_in_lift(lift.length)
        floor_4_to_lift(i, is_up_lift)
      } else {
        max_count_in_lift(lift.length)
        floor_4_to_lift(i + 1, is_up_lift)
      }
    }
  }

  def floor_5_to_lift(i: Int, is_up_lift: Boolean): Unit = {
    max_count_in_lift(lift.length)
    //    перевіряє чи є люди на прверсі 5, якщо є, то добаляємо у ліфт, якщо у ліфті менше 6-ти людей
    if (lift.length == 6) {
      max_count_in_lift(lift.length)
      println("Ліфт заповнений !")
    } else if (floor_5.isEmpty || i >= floor_5.length) {

    } else {
      if (floor_5(i).head > floor_5(i)(1) && floor_5(i)(2) == 0) {
        println("Людина зайшла у ліфт з п'ятого поверху.")
        max_count_in_lift(lift.length)
        lift = floor_5(i) :: lift
        waiting_times_down_floor_5 = floor_5(i)(3) :: waiting_times_down_floor_5
        floor_5 = floor_5.patch(i, Nil, 1)
        max_count_in_lift(lift.length)
        floor_5_to_lift(i, is_up_lift)
      }
    }
  }

  // -----------------------------------------------------------------------------------------------------------

  def lift_to_floor_1(i: Int): Unit = {
    if (i == lift.length || lift.isEmpty) {

    } else {
      if (lift(i)(1) == 1) {
        println("Людина вийшла з ліфту на перший поверх.")
        //        floor_1 = add_person_to_floor(1) :: floor_1
        lift = lift.patch(i, Nil, 1)

        lift_to_floor_1(i)
      }
      if (i < lift.length) {
        lift_to_floor_1(i + 1)
      }
    }
  }


  def lift_to_floor_2(i: Int): Unit = {
    if (i == lift.length || lift.isEmpty) {

    } else {
      if (lift(i)(1) == 2) {
        println("Людина вийшла з ліфту на другий поверх.")
        floor_2 = add_person_to_floor(2) :: floor_2
        lift = lift.patch(i, Nil, 1)

        lift_to_floor_2(i)
      }
      if (i < lift.length) {
        lift_to_floor_2(i + 1)
      }
    }
  }


  def lift_to_floor_3(i: Int): Unit = {
    if (i == lift.length || lift.isEmpty) {

    } else {
      if (lift(i)(1) == 3) {
        println("Людина вийшла з ліфту на третій поверх.")
        floor_3 = add_person_to_floor(3) :: floor_3
        lift = lift.patch(i, Nil, 1)

        lift_to_floor_3(i)
      }
      if (i < lift.length) {
        lift_to_floor_3(i + 1)
      }
    }
  }


  def lift_to_floor_4(i: Int): Unit = {
    if (i == lift.length || lift.isEmpty){

    }else{
      if (lift(i)(1) == 4){
        println("Людина вийшла з ліфту на четвертий поверх.")
        floor_4 = add_person_to_floor(4) :: floor_4
        lift = lift.patch(i, Nil, 1)

        lift_to_floor_4(i)
      }
      if (i < lift.length){
        lift_to_floor_4(i+1)
      }
    }
  }


  def lift_to_floor_5(i: Int): Unit = {
    if (i == lift.length || lift.isEmpty) {

    } else {
      if (lift(i)(1) == 5) {
        println("Людина вийшла з ліфту на п'ятий поверх.")
        floor_5 = add_person_to_floor(5) :: floor_5
        lift = lift.patch(i, Nil, 1)

        lift_to_floor_5(i)
      }
      if (i < lift.length) {
        lift_to_floor_5(i + 1)
      }
    }
  }

  // -----------------------------------------------------------------------------------------------------------

  def check_current_people(number_of_floor: Int, is_up_lift: Boolean): Unit = {
    //    перевірка мисиву певного поверху
    if (number_of_floor == 1) {
      lift_to_floor_1(0)
      floor_1_to_lift(0)
      if (is_up_lift) change_all_time()
    } else if (number_of_floor == 2) {
      lift_to_floor_2(0)
      floor_2_to_lift(0, is_up_lift)
      change_all_time()
    } else if (number_of_floor == 3) {
      lift_to_floor_3(0)
      floor_3_to_lift(0, is_up_lift)
      change_all_time()
    } else if (number_of_floor == 4) {
      lift_to_floor_4(0)
      floor_4_to_lift(0, is_up_lift)
      change_all_time()
    } else if (number_of_floor == 5) {
      lift_to_floor_5(0)
      floor_5_to_lift(0, is_up_lift)
      if (!is_up_lift) change_all_time()
    }
  }

  // -----------------------------------------------------------------------------------------------------------

  @tailrec
  def move_up_lift(number_of_floor: Int): Int = {
    if (number_of_floor > 5) {
      5
    }
    else {
      print_current_floor(number_of_floor)
      check_current_people(number_of_floor, true)
      move_up_lift(number_of_floor + 1)

    }
  }


  @tailrec
  def move_down_lift(number_of_floor: Int): Int = {
    if (number_of_floor == 0) {
      1
    }
    else {
      print_current_floor(number_of_floor)
      check_current_people(number_of_floor, false)
      move_down_lift(number_of_floor - 1)
    }
  }

  // -----------------------------------------------------------------------------------------------------------

  def administer_lift(number_of_floor: Int, number_of_cycles: Int): Unit = {
    if (number_of_cycles == 0){
      println("-----------------------")
    }else{
      var current_floor: Int = 0
      println("----------------------------------------")
      current_floor = move_up_lift(number_of_floor)
      current_floor = move_down_lift(current_floor - 1)
      println("----------------------------------------")
      administer_lift(number_of_floor, number_of_cycles-1)
    }
  }


  def main(args: Array[String]): Unit = {
    println("1 " + floor_1)
    println(administer_lift(1, 100000))
    print_all_floors()

    println()
    println("---------------------------------")
    println("Загальна кількість часу: "+COMMON_TIME+" сек")
    println("Максимальна кількість людей у ліфті: "+MAX_COUNT_IN_LIFT)
    println()
    print_all_avg_time()

  }

}