package chapter2.PriorityQueue

import java.util

import chapter2.SortBasic

import scala.collection.mutable
import scala.reflect.ClassTag

trait PriorityQueue[T] {
  def isEmpty: Boolean
  def size: Int
  def insert(key:T):Unit
  def delMax():T
  def delMin():T
}

class UnorderedMaxPQ[T >:Null <:Comparable[T] :ClassTag](val maxN:Int) extends SortBasic[T] with PriorityQueue[T] {
  private var pq = new Array[T](maxN)
  private var N = 0;
  override def isEmpty: Boolean = N == 0
  override def size: Int = N
  override def insert(key: T): Unit = { pq(N) = key; N += 1 }
  override def delMax(): T = {
    var maxIndex = N - 1
    (0 until N).foreach { i =>
      val now = pq(i) //now can't be null
      val check = now.compareTo(pq(maxIndex))
      if (check > 0) {
        maxIndex = i
        exch(pq, i, N-1)
      }
    }
    val ans = pq(N-1)
    pq(N-1) = null
    N -= 1
    ans
  }
  private def delMaxVersion1(): T = {
    val max = pq.length - 1
    pq.indices.foreach { i =>
      val now = pq(i)
      val t = pq(max)
      //println("now t", now, t)
      if (t == null) {
        pq(max) = now
      } else {
        if (!(now == null) && now.compareTo(t) > 0) {
          //println("exch",i,0)
          exch(pq, i, max)
          //println(pq.mkString(","))
        }
      }
    }
    N -= 1
    val res = pq(max)
    pq(max) = null
    res
  }
  override def delMin(): T = throw new UnsupportedOperationException("don't support delMin")
}

object UnorderedMaxPQTest extends App {
  val item = Seq("A","B","D","E","Q","T","E").toArray
  val pq = new UnorderedMaxPQ[String](20)
  item.foreach { i =>
    pq.insert(i)
    //println(pq.pq.mkString(","))
  }
  println(pq.delMax())
  println(pq.delMax())
  println(pq.size)
  //println(pq.pq.mkString("<, "))
}

class MaxPQ[T >:Null <:Comparable[T] :ClassTag](val maxN:Int) extends SortBasic[T] with PriorityQueue[T] {
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

  override def delMin(): T = throw new UnsupportedOperationException("don't support it")
}

class MaxPQ3[T >:Null <:Comparable[T] :ClassTag](val maxN:Int) extends SortBasic[T] {
  val pq = new Array[T](maxN)
  var N = 0
  def isEmpty: Boolean = N == 0
  def size: Int = N
  def insert(key: T): Unit = {
    //println("Insert " + key + ", n now is " + N + ", N++ and set array(n) = key and swim(" + (N + 1) + ")")
    N += 1; pq(N) = key;
    println("before swim" -> pq.mkString(","))
    swim(N);
  }
  def delMax(): T = {
    val headOld = pq(1)
    pq(1) = pq(N)
    pq(N) = null
    N -= 1
    sink(1)
    headOld
  }
  private def swim(m:Int): Unit = {
    var k = m
    while (k > 3 && less(pq((k-1)/3), pq(k))) {
      //println("exch now...")
      exch(pq,(k-1)/3,k)
      //println("now is ",pq.mkString(", "))
      k = (k-1)/3
    }
  }
  private def sink(m:Int): Unit = {
    var k = m
    var end = false
    while (!end && (3 * k + 1) <= N) {
      var j = k * 3 + 1
      val (ja,jb,jc,jk) = (pq(j),pq(j+1),pq(j+2),pq(k))
      if (j + 2 <= N && less(jk,jc)) j += 2
      else if (j + 1 <= N && less(jk,jb)) j += 1
      if (less(ja,jk)) { end = true }
      else { exch(pq, k, j); k = j }
    }
  }
}

object Test extends App {
  val pq = new MaxPQ3[String](10)
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

class IndexMinPQ[T >: Null <:Comparable[T] :ClassTag](val maxN:Int) extends SortBasic[T] {

  def insert(k:Int, item:T): Unit = ???
  def change(k:Int, item:T): Unit = ???
  def contains(item:T): Int = ???
  def delete(item:T): Unit = ???
  def min: T = ???
  def minIndex: Int = ???
  def delMin(): Int = ???
  def isEmpty: Boolean = ???
  def size: Int = ???
}

class HeapSort[T >: Null <: Comparable[T] :ClassTag] extends SortBasic[T] {
  def sort(a:Array[T]):Array[T] = {
    val pq = mutable.PriorityQueue.from(a)
    pq.dequeueAll.toArray
  }
}

class HeapSortInplace[T >:Null <:Comparable[T] :ClassTag] extends SortBasic[T] {
  def sort(pq:Array[T]):Unit = {
    var N = pq.length - 1
    (1 to N/2).reverse.foreach { k =>
      //println("k is",k,pq(k))
      //println("before sink", pq.mkString(","))
      sink(pq, N, k)
      //println("after sink", pq.mkString(","))
    }
    println(pq.mkString(", "))
    while (N > 1) {
      exch(pq, N, 1)
      N -= 1
      sink(pq, N, 1)
    }
  }
  private def sink(pq:Array[T], N:Int, m:Int): Unit = {
    var k = m
    var end = false
    //println("sink: k, n",m,N)
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

object Test2 extends App {
  val a = Array(null,"H","E","L","L","O")
  new HeapSortInplace[String].sort(a)
  println(a.mkString(", "))
}