package week4

import edu.princeton.cs.algs4.{MaxPQ, MinPQ}

object MedianFindDemo extends App {
  val a = new MinPQ[Int]
  val b = new MaxPQ[Int]
  val item = Array(1,1,6,3,8,2,9,12,32,44,17)
  val dm = new DynamicMedianInt
  item.foreach(dm.insert)
  println("Median of " + item.sorted.mkString(", ") + s" is ${dm.median}")
}

class DynamicMedianInt {
  val minPQ = new MinPQ[Int]()
  val maxPQ = new MaxPQ[Int]()
  //maxPQ head 负责维护中位数，当新数大于它，则放到 minPQ 中，反之则放在 maxPQ 中
  //每插入一个数，通过调整两边堆的大小来保证 maxPQ head 始终为中位数
  def insert(n:Int): Unit = {
    if (maxPQ.isEmpty) maxPQ.insert(n)
    else {
      if (n > maxPQ.max()) minPQ.insert(n)
      else maxPQ.insert(n)
    }
    if (maxPQ.size() + 1 == minPQ.size())
      maxPQ.insert(minPQ.delMin())
    if (maxPQ.size() == minPQ.size() + 2)
      minPQ.insert(maxPQ.delMax())
  }
  def median: Int = maxPQ.max()
  def removeMedian(): Int = maxPQ.delMax()
}