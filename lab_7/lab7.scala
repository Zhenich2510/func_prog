import lab7.file_path

import java.io._
import scala.io.Source

object lab7 {
  val file_path = "new_file"

  def write_to_txt(txt:String, file_path:String):Unit={
    val writer = new PrintWriter(new File(file_path))
    writer.write(txt)
    writer.close()
  }


  var new_txt = ""

  def iter(i:Int, cur:Int):Any={
    if (i == cur){
      write_to_txt(new_txt, file_path)
      return 0
    }

    val text = scala.io.StdIn.readLine("Пишіть текст -> ")
    new_txt += text + "\n"

    iter(i, cur+1)
  }

  def print_file(file_path:String):Any={
    Source.fromFile(file_path).foreach{print}
  }


  var count:Int = 0
  var text:String = ""
  val digits: Set[Char] = Set('1', '2', '3', '4', '5', '6', '7', '8', '9', '0')

  def count_numeric_chars(file_path:String, i:Int):Unit={

    if (i == 0){
      Source.fromFile(file_path).foreach {
        text += _
      }
    }
    if (i == text.length()) {
      return 0
    }

    if (digits.contains(text(i))) {
      count += 1
    }

    count_numeric_chars(file_path, i+1)

  }






  def main(args: Array[String]): Unit = {

    val len = scala.io.StdIn.readLine("Введіть кількість строк -> ")

    iter(len.toInt, 0)
    println()

    print_file(file_path)

    count_numeric_chars(file_path, 0)

    write_to_txt(count.toString, "count")

    println()

    print_file("count")

  }
}

