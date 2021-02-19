package chapter2

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

abstract class SortBasic[T <: Comparable[T]] {
  def less(v:T,w:T): Boolean = v.compareTo(w) < 0
  def exch(a:Array[T], i:Int, j:Int): Unit = {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }
  def isSorted(a:Array[T]): Boolean = {
    (1 to a.length).foreach { i =>
      if (less(a(i),a(i-1))) return false
    }; true
  }
}

class Selection[T<: Comparable[T]] extends SortBasic[T] {
  def sort(a:Array[T]): Unit = {
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

class Insert[T<: Comparable[T]] extends SortBasic[T] {
  def sort(a:Array[T]): Unit = {
    (1 until a.length).foreach { i =>
      (1 to i).reverse.foreach { j =>
        if (less(a(j),a(j-1))) exch(a, j, j-1)
      }
    }
  }
}

class Shell[T<: Comparable[T]] extends SortBasic[T] {
  def sort(a:Array[T]): Unit = {
    val N = a.length
    val hs = (1 to N).foldLeft(Array(1)) {
      (sum, now) => if (now == sum.last * 3 + 1) sum :+ now else sum }
    hs.reverse.foreach { h =>
      (h until N).foreach { i =>
        (1 to i).reverse.foreach { j => //此处实现错误，应该是 step h
          if ((j - h >= 0) && less(a(j), a(j - h))) {
            exch(a, j, j - h)
          }
        }
      }
    }
  }
}

class Shuffle[T<: Comparable[T]] extends SortBasic[T] {
  def shuffle(a:Array[T]): Unit = {
    a.indices.foreach { i =>
      exch(a, i, Random.nextInt(i + 1))
    }
  }
}

class Merge[T<: Comparable[T] :ClassTag] extends SortBasic[T] {
  private var aux: Array[T] = _
  //原地归并：首先构造两个一样的数组，然后对索引范围内的值进行分半，使用指针依次进行两边头部值的判断。
  //如果指针指向的右边小于左边，则将右边的值复制到目的数组指针位置（下称接收），同时将其局部索引增加，局部数组的头指向下一个未被选中的值。
  //反之，则接收左边的值，同时将其局部索引增加，指向局部数组下一个较小的头。如果哪一边遍历完毕（即左右两部分不等宽），那么就自动切换到另一边进行遍历
  //原地归并要求局部数组必须有序，因为其选择某个值的时候，必须保证这个值在其局部数组最小，被淘汰的值在其局部数组最小
  private def merge(a:Array[T], lo: Int, mid: Int, hi: Int): Unit = {
    var (i,j) = (lo, mid + 1)
    Array.copy(a,lo,aux,lo,hi - lo + 1)
    (lo to hi).foreach { k =>
      if (i > mid) {
        a(k) = aux(j)
        j += 1
      } else if (j > hi) {
        a(k) = aux(i)
        i += 1
      } else if (less(aux(j),aux(i))) {
        a(k) = aux(j)
        j += 1
      } else {
        a(k) = aux(i)
        i += 1
      }
    }
  }
  private def sort(a:Array[T], lo: Int, hi: Int): Unit = {
    if (lo >= hi) return
    val mid = lo + (hi - lo)/2
    sort(a, lo, mid)
    sort(a, mid + 1, hi)
    //最内层：将两个值进行大小比较和排序
    //最外层：最后一次将最大的两个拍过序的数组合并
    merge(a, lo, mid, hi)
  }
  def sort(a:Array[T]): Unit = {
    aux = new Array[T](a.length)
    sort(a,0,a.length - 1)
  }
}

class MergeHalf[T<: Comparable[T] :ClassTag] extends SortBasic[T] {
  private def merge(a:Array[T], aux:Array[T], lo: Int, mid: Int, hi: Int): Unit = {
    var (i,j) = (lo, mid + 1)
    (lo to hi).foreach { k =>
      if (i > mid) {
        aux(k) = a(j)
        j += 1
      } else if (j > hi) {
        aux(k) = a(i)
        i += 1
      } else if (less(a(j),a(i))) {
        aux(k) = a(j)
        j += 1
      } else {
        aux(k) = a(i)
        i += 1
      }
    }
  }
  private def sort(a:Array[T], aux:Array[T], lo: Int, hi: Int): Unit = {
    if (lo >= hi) return
    val mid = lo + (hi - lo)/2
    sort(aux, a, lo, mid)
    sort(aux, a, mid + 1, hi)
    merge(a, aux, lo, mid, hi)
  }
  def sort(a:Array[T]): Unit = {
    val aux = a.clone()
    sort(aux, a, 0,a.length - 1)
  }
}

class MergeBU[T<: Comparable[T] :ClassTag] extends SortBasic[T] {
  private var aux: Array[T] = _
  private def merge(a:Array[T], lo: Int, mid: Int, hi: Int): Unit = {
    var (i,j) = (lo, mid + 1)
    Array.copy(a,lo,aux,lo,hi - lo + 1)
    (lo to hi).foreach { k =>
      if (i > mid) {
        a(k) = aux(j)
        j += 1
      } else if (j > hi) {
        a(k) = aux(i)
        i += 1
      } else if (less(aux(j),aux(i))) {
        a(k) = aux(j)
        j += 1
      } else {
        a(k) = aux(i)
        i += 1
      }
    }
  }
  def sort(a:Array[T]): Unit = {
    aux = new Array[T](a.length)
    val N = a.length
    def szStream(start:Int):Stream[Int] = start #:: szStream(start + start)
    def stepStream(start:Int,sz:Int):Stream[Int] = start #:: stepStream(start + sz + sz,sz)
    szStream(1).takeWhile(_ < N).foreach { sz =>
      //1,2,4,8,16,...
      stepStream(0, sz).takeWhile(_ < N - sz).foreach { lo =>
        //for sz1: 0,2,4,6,8
        merge(a,lo,lo+sz-1,Math.min(lo+sz+sz-1,N-1))
      }
    }

  }
}

object Test extends App {
  /*val array = Array(1,1233,523,5,6,3,4,123,12,3,21).map(new Integer(_))
  new Shuffle[Integer]().shuffle(array)
  println(array.mkString(", "))*/
  val m = new MergeHalf[Integer]()
  val ar = Array(1,223,23,14,23,543,545,6,34,4532,4,2,3).map(new Integer(_))
  m.sort(ar)
  println(ar.mkString(", "))
}
