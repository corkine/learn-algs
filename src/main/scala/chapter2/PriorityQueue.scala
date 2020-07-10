package chapter2.PriorityQueue

import chapter2.SortBasic

import scala.reflect.ClassTag

class MaxPQ[T >:Null <:Comparable[T] :ClassTag](val maxN:Int) extends SortBasic[T] {
  val pq = new Array[T](maxN)
  var N = 0 //p0 没有使用，因此其等于数组长度，也等于数的个数
  def isEmpty: Boolean = N == 0
  def size: Int = N
  def insert(key: T): Unit = {
    N += 1; pq(N) = key; swim(N);
  }
  def delMax(): T = {
    val headOld = pq(1)
    pq(1) = pq(N)
    pq(N) = null
    N -= 1
    sink(1) //sink 要先将 N 状态改变后再操作，否则可能误用 N
    headOld
  }
  private def swim(m:Int): Unit = {
    var k = m
    while (k > 1 && less(pq(k/2), pq(k))) {
      exch(pq, k/2, k)
      k = k / 2
    }
  }
  private def sink(m:Int): Unit = {
    var k = m
    var end = false
    while (!end && 2 * k <= N) {
      var j = k * 2
      if (j < N && less(pq(j),pq(j + 1))) j += 1
      if (less(pq(k),pq(j))) {
        exch(pq, k, j)
        k = j
      } else end = true
    }
  }
}

object Test extends App {
  val pq = new MaxPQ[String](10)
  pq.insert("H")
  pq.insert("H")
  pq.insert("L")
  pq.insert("W")
  pq.insert("T")
  pq.delMax()
  pq.insert("Q")
  pq.insert("E")
  pq.delMax()
  println(pq.size)
  println(pq.pq.mkString(", "))
}
