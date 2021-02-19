package chapter2

class QuickSort[T <: Comparable[T]] extends SortBasic[T] {
  def sort(a: Array[T]): Unit = {
    new Shuffle[T].shuffle(a)
    sort(a, 0, a.length - 1)
  }
  //核心思想：选择一个值，这个值左边均小于它，右边均大于它，这样的话，递归选择局部的值重复这一过程
  //整个数组就是有序的。（此处的选择并非单独查找，而是交换一些位置来达到目的）
  def sort(a: Array[T], from: Int, to: Int): Unit = {
    if (to <= from) return
    val j = partition(a, from, to)
    sort(a, from, j - 1)
    sort(a, j + 1, to)
  }

  def select(a: Array[T], k: Int): T = {
    new Shuffle[T].shuffle(a)
    var (find, lo, hi) = (false, 0, a.length - 1)
    while (!find && (hi > lo)) {
      val j = partition(a, lo, hi)
      if (k > j) lo = j + 1
      else if (k < j) hi = j - 1
      else find = true
    }
    a(k)
  }

  def partition(a: Array[T], lo: Int, hi: Int): Int = {
    var (i, j) = (lo, hi + 1)
    val v = a(i)
    var (leftEnd, rightEnd, allEnd) = (false, false, false)
    while (!allEnd) {
      while ({ { //从左边找到每一个较大的值
        i += 1 //从选中值后第一个元素开始，不用从 lo 开始，否则浪费一次比较
        if (i == hi) leftEnd = true //边界条件，如果一直找不到，则退出循环
      } ; !leftEnd && less(a(i),v)}) ()
      while ({ { //从右边找到每一个较小的值
        j -= 1 //从最高位开始 - hi
        if (j == lo) rightEnd = true //边界条件，如果向左一直找不到，则退出循环
        //注意，这里找不到一定要到 lo，而非 lo + 1，否则在小大两个排序就会出问题
      } ; !rightEnd && less(v,a(j))}) ()
      if (i >= j) allEnd = true //对于边界情况直接返回，比如常数数组，排过序的数组
      else exch(a, i, j) //将小的放左边，大的放右边
    }
    exch(a, lo, j) //将被比较元素拿出来放在 j 为止，至此此元素左边均为小于它的值，右边均为大于它的值
    j //返回这个分界线，以便于递归进行子串的继续 partition
  }
  //对于常数组，直接不排序，对于排过序的数组，直接不排序，对于逆序数组，则直接将最大值转移到最右边，且指向它，也不排序。
  //对于一些极端情况，比如 1 10 9 8 7 6 这样， 最后拆分结果还是 1 10 9 8 7 6，且指针移向第一个值。
  //对于小大两个比较，最后还是小大，且指向小，对于大小两个比较，最后是小大且指向大。
}

class ThreeWayQuickSort[T <: Comparable[T]] extends SortBasic[T] {
  def sort(a:Array[T], lo: Int, hi: Int): Unit = {
    if (hi <= lo) return
    var (lt, gt, i, v) = (lo, hi, lo, a(lo))
    while (i <= gt) {
      val cmp = a(i).compareTo(v)
      if      (cmp < 0) { exch(a, lt, i); lt += 1; i += 1}
      else if (cmp > 0) { exch(a, i, gt); gt -= 1 }
      else    i += 1
    }
    sort(a, lo, lt - 1)
    sort(a, gt + 1, hi)
  }
  def sort(a:Array[T]): Unit = {
    new Shuffle[T].shuffle(a)
    sort(a,0,a.length - 1)
  }
}

class ThreeWayQuickSort2[T <: Comparable[T]] extends SortBasic[T] {
  def sort(a:Array[T], lo: Int, hi: Int): Unit = {
    if (hi <= lo) return
    var (lt, gt, i, v) = (lo, hi, lo, a(lo))
    println(s"now lo $lo, hi $hi, comp with value: $v")
    while (i <= gt) {
      println(a.zipWithIndex.map { case (t,inow) =>
        val sb = new StringBuilder
        sb.append("(").append(t).append(")").append(":").append(inow)
        if (inow == i)  sb.append(":i")
        if (inow == lt) sb.append(":lt")
        if (inow == gt) sb.append(":gt")
        sb.toString()
      }.mkString(", "))
      val cmp = a(i).compareTo(v)
      if      (cmp < 0) { exch(a, lt, i); lt += 1; i += 1}
      else if (cmp > 0) { exch(a, i, gt); gt -= 1 }
      else    i += 1
    }
    println(s"now sort subarray from $lo to ${lt - 1}")
    sort(a, lo, lt - 1)
    println(s"now sort subarray from ${gt+1} to $hi")
    sort(a, gt + 1, hi)
  }
  def sort(a:Array[T]): Unit = {
    new Shuffle[T].shuffle(a)
    println("Shuffled array " + a.mkString(", "))
    sort(a,0,a.length - 1)
  }
}

object Test2 extends App {
  val item = Array("H","E","L","L","O","W","O","R","L","D")
  new ThreeWayQuickSort2[String].sort(item)
  println(item.mkString(", "))
}

