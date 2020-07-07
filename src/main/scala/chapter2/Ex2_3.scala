package chapter2.Ex2_3

import chapter2.{Shuffle, SortBasic, ThreeWayQuickSort}

object N1 extends App {
  class QuickSortDebug[T <: Comparable[T]] extends SortBasic[T] {
    def sort(a: Array[T]): Unit = {
      new Shuffle[T].shuffle(a)
      println("Shuffled array is " + a.mkString(", "))
      sort(a, 0, a.length - 1)
    }
    //核心思想：选择一个值，这个值左边均小于它，右边均大于它，这样的话，递归选择局部的值重复这一过程
    //整个数组就是有序的。（此处的选择并非单独查找，而是交换一些位置来达到目的）
    def sort(a: Array[T], from: Int, to: Int): Unit = {
      if (to <= from) return
      val j = partition(a, from, to)
      //println(s"now handle with ($from to ${j - 1})")
      sort(a, from, j - 1)
      //println(s"now handle with (${j + 1} to $to)")
      sort(a, j + 1, to)
    }

    def select(a: Array[T], k: Int): T = {
      new Shuffle[T].shuffle(a)
      println("Shuffled array is " + a.mkString(", "))
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
      println(s"($lo to $hi) with select index $i value $v")
      var (leftEnd, rightEnd, allEnd) = (false, false, false)
      while (!allEnd) {
        println({
          a.zipWithIndex.map { case (t,now) =>
            val sb = new StringBuilder()
            sb.append("(").append(t).append(")").append(":").append(now)
            if (now == i) sb.append(":i")
            if (now == j) sb.append(":j")
            if (now + 1 == j && now + 1 == a.length) sb.append("  END:j")
            sb.toString()
          }.mkString(", ")
        })
        do { //从左边找到每一个较大的值
          i += 1 //从选中值后第一个元素开始，不用从 lo 开始，否则浪费一次比较
          if (i == hi) leftEnd = true //边界条件，如果一直找不到，则退出循环
        } while (!leftEnd && less(a(i),v))
        do { //从右边找到每一个较小的值
          j -= 1 //从最高位开始 - hi
          if (j == lo) rightEnd = true //边界条件，如果向左一直找不到，则退出循环
          //注意，这里找不到一定要到 lo，而非 lo + 1，否则在小大两个排序就会出问题
        } while (!rightEnd && less(v,a(j)))
        if (i >= j) allEnd = true //对于边界情况直接返回，比如常数数组，排过序的数组
        else exch(a, i, j) //将小的放左边，大的放右边
      }
      println({
        a.zipWithIndex.map { case (t,now) =>
          val sb = new StringBuilder()
          sb.append("(").append(t).append(")").append(":").append(now)
          if (now == i) sb.append(":i")
          if (now == j) sb.append(":j")
          if (now + 1 == j && now + 1 == a.length) sb.append("  END:j")
          sb.toString()
        }.mkString(", ")
      })
      exch(a, lo, j) //将被比较元素拿出来放在 j 为止，至此此元素左边均为小于它的值，右边均为大于它的值
      j //返回这个分界线，以便于递归进行子串的继续 partition
    }
    //对于常数组，直接不排序，对于排过序的数组，直接不排序，对于逆序数组，则直接将最大值转移到最右边，且指向它，也不排序。
    //对于一些极端情况，比如 1 10 9 8 7 6 这样， 最后拆分结果还是 1 10 9 8 7 6，且指针移向第一个值。
    //对于小大两个比较，最后还是小大，且指向小，对于大小两个比较，最后是小大且指向大。
  }

  val items: Array[String] = Array("E","A","S","Y","Q","U","E","S","T","I","O","N")
  new QuickSortDebug[String].sort(items)
  println("final result: " + items.mkString(", "))

  /*(0 to 11) with select index 0 value O
  (O):0:i, (Y):1, (N):2, (I):3, (Q):4, (E):5, (S):6, (S):7, (E):8, (U):9, (T):10, (A):11  END:j
  (O):0, (A):1:i, (N):2, (I):3, (Q):4, (E):5, (S):6, (S):7, (E):8, (U):9, (T):10, (Y):11:j
  (O):0, (A):1, (N):2, (I):3, (E):4:i, (E):5, (S):6, (S):7, (Q):8:j, (U):9, (T):10, (Y):11
  (O):0, (A):1, (N):2, (I):3, (E):4, (E):5:j, (S):6:i, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (0 to 4) with select index 0 value E
  (E):0:i, (A):1, (N):2, (I):3, (E):4, (O):5:j, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (E):0, (A):1, (E):2:i, (I):3, (N):4:j, (O):5, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (E):0, (A):1, (E):2:j, (I):3:i, (N):4, (O):5, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (0 to 1) with select index 0 value E
  (E):0:i, (A):1, (E):2:j, (I):3, (N):4, (O):5, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (E):0, (A):1:i:j, (E):2, (I):3, (N):4, (O):5, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (3 to 4) with select index 3 value I
  (A):0, (E):1, (E):2, (I):3:i, (N):4, (O):5:j, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (A):0, (E):1, (E):2, (I):3:j, (N):4:i, (O):5, (S):6, (S):7, (Q):8, (U):9, (T):10, (Y):11
  (6 to 11) with select index 6 value S
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (S):6:i, (S):7, (Q):8, (U):9, (T):10, (Y):11  END:j
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (S):6, (Q):7:i, (S):8:j, (U):9, (T):10, (Y):11
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (S):6, (Q):7:j, (S):8:i, (U):9, (T):10, (Y):11
  (8 to 11) with select index 8 value S
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (Q):6, (S):7, (S):8:i, (U):9, (T):10, (Y):11  END:j
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (Q):6, (S):7, (S):8:j, (U):9:i, (T):10, (Y):11
  (9 to 11) with select index 9 value U
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (Q):6, (S):7, (S):8, (U):9:i, (T):10, (Y):11  END:j
  (A):0, (E):1, (E):2, (I):3, (N):4, (O):5, (Q):6, (S):7, (S):8, (U):9, (T):10:j, (Y):11:i
  final result: A, E, E, I, N, O, Q, S, S, T, U, Y*/

}

object N2 {
  //最大的元素最多被交换 N - 1 次，当且仅当其作为第一轮的被选中值时，j 想要找更小的值，其找了 N - 1 次，且每次都将右边的值扔到左边
}


object N3 extends App {
  val sort = new ThreeWayQuickSort[String]
  val items = Array("H","H","L","L","H","L","H","L")
  //sort.sort(items)
  println(items.mkString(", "))
  def sort(in:Array[String]):Unit = {
    var (lastIndex,i,select) = (in.length - 1,0,in(0))
    while (i <= lastIndex) {
      val now = in(i)
      if (now != select) {
        val tempLast = in(lastIndex)
        in(lastIndex) = now
        in(i) = tempLast
        lastIndex -= 1
      } else {
        i += 1
      }
    }
  }
  sort(items)
  println(items.mkString(","))
}

object N4 {
  //逆序输入
  //N8 - 1/2N^2 1 + 2 + 3 + .. + N
  //N11 常数列
  //N12 使用 C 作为拆分，因为 A B 重复过多浪费大量计算
  //N13 2^m = N 最差为每次均轮空，则为 2^N/2
}

