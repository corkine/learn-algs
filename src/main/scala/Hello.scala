package week5 

object App {
  enum ABC:
    case A,B,C
  def main(args: Array[String]): Unit = {
    println(s"HELLO WORLD ${ABC.A}")
  }
}
