package chapter3.Hello

object Main extends App {
(scala.io.StdIn.readLine("Input data >>")
  .split(" ")
  .foldLeft(Map[Int,Int]()) { (a, b) => a.updated(b.length,a.getOrElse(b.length,0) + 1)}
  .toBuffer:collection.mutable.Buffer[(Int,Int)])
  .sortBy(_._1)
  .foreach(r => println(s"[%3s]${"=" * r._2 * 5}".format(r._1)))
}
