package chapter2

class QuickSort[T <: Comparable[T]] extends SortBasic[T] {
  def sort(a:Array[T]): Unit = {
    new Shuffle[T].shuffle(a)
    sort(a, 0, a.length - 1)
  }
  def sort(a:Array[T],from:Int,to:Int): Unit = {
    if (to <= from) return
    val j = partition(a, from, to)
    sort(a, from, j - 1)
    sort(a, j, to)
  }
  def partition(a: Array[T], lo: Int, hi: Int): Int = {
    ???
  }
}
