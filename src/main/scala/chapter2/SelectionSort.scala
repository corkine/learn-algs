package chapter2

import scala.util.Random

trait SortBasic {
  def less[T <:Comparable[T]](v:T,w:T): Boolean = v.compareTo(w) < 0
  def exch[T <:Comparable[T]](a:Array[T], i:Int, j:Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }
  def isSorted[T <: Comparable[T]](a:Array[T]): Boolean = {
    (1 to a.length).foreach { i =>
      if (less(a(i),a(i-1))) return false
    }; true
  }
}
object SortUtils extends SortBasic

trait Selection extends SortBasic {
  def sort[T <:Comparable[T]](a:Array[T]): Unit = {
    val N = a.length
    a.indices.foreach { i =>
      //println("Now is ",i, "N is ",N)
      var min = i
      ((i+1) until N).foreach { j =>
        if (less(a(j),a(min))) min = j
      }
      exch(a, i, min)
    }
  }
}

trait Insert extends SortBasic {
  def sort[T <:Comparable[T]](a:Array[T]): Unit = {
    (1 until a.length).foreach { i =>
      (1 to i).reverse.foreach { j =>
        if (less(a(j),a(j-1))) exch(a, j, j-1)
      }
    }
  }
}

trait Shell extends SortBasic {
  def sort[T <:Comparable[T]](a:Array[T]): Unit = {
    val N = a.length
    val hs = (1 to N).foldLeft(Array(1)) {
      (sum, now) => if (now == sum.last * 3 + 1) sum :+ now else sum }
    hs.reverse.foreach { h =>
      (h until N).foreach { i =>
        (1 to i).reverse.foreach { j =>
          if ((j - h >= 0) && less(a(j), a(j - h))) {
            exch(a, j, j - h)
          }
        }
      }
    }
  }
}

trait Shuffle extends SortBasic {
  def shuffle[T <:Comparable[T]](a:Array[T]): Unit = {
    a.indices.foreach { i =>
      exch(a, i, Random.nextInt(i + 1))
    }
  }
}

object Test extends App with Shuffle {
  val array = Array(1,1233,523,5,6,3,4,123,12,3,21).map(new Integer(_))
  shuffle(array)
  println(array.mkString(", "))
}
