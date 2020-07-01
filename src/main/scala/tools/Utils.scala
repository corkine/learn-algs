package tools

object Utils {
  def withStringBuilder(op:StringBuilder => Unit):String = {
    val sb = new StringBuilder()
    op(sb)
    sb.toString()
  }
}
