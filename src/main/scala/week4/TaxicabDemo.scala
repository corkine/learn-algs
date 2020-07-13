package week4

sealed case class Taxicab1(a:Int,b:Int) extends Ordered[Taxicab1] {
  val sum: Double = Math.pow(a,3) + Math.pow(b,3)
  override def compare(that: Taxicab1): Int = -1*this.sum.compare(that.sum)
  override def toString: String = s"[Taxicab]($a,$b:$sum)"
}

object TaxicabDemo extends App {
  val N = 100
  val pq = collection.mutable.PriorityQueue.empty[Taxicab1]
  (1 to N).foreach { i =>
    pq.enqueue(Taxicab1(i,i))
  }
  var prev = Taxicab1(0,0)
  while (pq.nonEmpty) {
    val now = pq.dequeue()
    if (now.compare(prev) == 0) {
      println(s"Find $prev vs. $now")
    }
    prev = now
    if (now.b < N) {
      pq.enqueue(Taxicab1(now.a,now.b + 1))
    }
  }
}

