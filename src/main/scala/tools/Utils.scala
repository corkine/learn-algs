package tools

object Utils {
  def withStringBuilder(op:StringBuilder => Unit):String = {
    val sb = new StringBuilder()
    op(sb)
    sb.toString()
  }
  def time(repeat:Int)(op: => Unit):String = {
    val start = System.currentTimeMillis()
    val sb = new StringBuilder
    sb.append(s"Timing for $repeat times ==========================\n")
    (1 to repeat).foreach { i =>
      val now = System.currentTimeMillis()
      op
      val cost = System.currentTimeMillis() - now
      sb.append("| " * 1).append(s"C$i cost ${cost * 1.0/1000} seconds.").append("\n")
    }
    val time = System.currentTimeMillis() - start
    sb.append("| " * 1).append(s"Total cost ${time * 1.0/1000} seconds.").append("\n")
      .append("| " * 1).append(s"Average cost ${(time * 1.0/(1000 * repeat)).formatted("%.3f")} seconds.").append("\n")
        .append("=============================================").append("\n")
    sb.toString()
  }
  def time1(op: => Unit): String = time(1)(op)
  def ptime1(op: => Unit): Unit = println(time(1)(op))
  def time3(op: => Unit): String = time(3)(op)
  def ptime3(op: => Unit): Unit = println(time(3)(op))
  def btime1(op: => Unit)(implicit sb:StringBuilder): Unit = sb.append(time(1)(op))
  def btime3(op: => Unit)(implicit sb:StringBuilder): Unit = sb.append(time(3)(op))

  private var data:String = ""
  def vtime1(op: => Unit): Unit = data += time(1)(op)
  def vtime3(op: => Unit): Unit = data += time(3)(op)
  def showResult(): Unit = println(data)
  def showResultWithReset(): Unit = { println(data); data = "" }
  def getResult: String = data
  def getResultWithReset: String = { val t = data; data = ""; t }
  def clear(): Unit = data = ""
}