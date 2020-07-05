package chapter2

import java.lang

import tools.Utils

import scala.util.Random

object N1 {
  //E A S Y Q U E S T I O N
  //(E) [A] S Y Q U E S T I O N
  //(A) E S Y Q U E S T I O N
  //(A) E [S] Y Q U E S T I O N
  //(A) E S [Y] Q U E S T I O N
  //(A) E S Y [Q] U E S T I O N
  //(A) E S Y Q [U] E S T I O N
  //(A) E S Y Q U [E] S T I O N
  //(A) E S Y Q U E [S] T I O N
  //(A) E S Y Q U E S [T] I O N
  //(A) E S Y Q U E S T [I] O N
  //(A) E S Y Q U E S T I [O] N
  //(A) E S Y Q U E S T I O [N]
  //A (E) [S] Y Q U E S T I O N
  //...
  //A E (S) [Y] Q U E S T I O N
  //...
  //A E (E) Y Q U S S T I O N
  //A E E (Y) [Q] U S S T I O N
  //... final
  //A E E I N O Q S S T U Y
}

object N2 {
  //选择排序一个元素最极端情况是它第一个，然后被比较了 N-1 次
  //每次排序，每个元素最多被交换 N-1 次，最少被交换 0 次，随着向右移动，其最多被交换次数依次下降，
  //N 个元素则是 ((N-1) + (N-2) + ... + 0)/2 约等于 N*N/2 - (N+1)N/4 次。
}

object N3 {
  //选择排序针对完全不重复的情况处理最不占劣势
  //10 9 8 7 6 5 4 3 2 1 0
}

object N4 extends App {
  class InsertExp[T<:Comparable[T]] extends SortBasic[T] {
    def sort(a:Array[T]): Unit = {
      (1 until a.length).foreach { i =>
        (1 to i).reverse.foreach { j =>
          if (less(a(j),a(j-1))) {
            println(i,a.mkString(","))
            exch(a, j, j - 1)
          }
        }
      }
    }
  }
  //new InsertExp[String]().sort(Array("E","A","S","Y","Q","U","E","S","T","I","O","N"))
  /*(1,E,A,S,Y,Q,U,E,S,T,I,O,N)
  (4,A,E,S,Y,Q,U,E,S,T,I,O,N)
  (4,A,E,S,Q,Y,U,E,S,T,I,O,N)
  (5,A,E,Q,S,Y,U,E,S,T,I,O,N)
  (6,A,E,Q,S,U,Y,E,S,T,I,O,N)
  (6,A,E,Q,S,U,E,Y,S,T,I,O,N)
  (6,A,E,Q,S,E,U,Y,S,T,I,O,N)
  (6,A,E,Q,E,S,U,Y,S,T,I,O,N)
  (7,A,E,E,Q,S,U,Y,S,T,I,O,N)
  (7,A,E,E,Q,S,U,S,Y,T,I,O,N)
  (8,A,E,E,Q,S,S,U,Y,T,I,O,N)
  (8,A,E,E,Q,S,S,U,T,Y,I,O,N)
  (9,A,E,E,Q,S,S,T,U,Y,I,O,N)
  (9,A,E,E,Q,S,S,T,U,I,Y,O,N)
  (9,A,E,E,Q,S,S,T,I,U,Y,O,N)
  (9,A,E,E,Q,S,S,I,T,U,Y,O,N)
  (9,A,E,E,Q,S,I,S,T,U,Y,O,N)
  (9,A,E,E,Q,I,S,S,T,U,Y,O,N)
  (10,A,E,E,I,Q,S,S,T,U,Y,O,N)
  (10,A,E,E,I,Q,S,S,T,U,O,Y,N)
  (10,A,E,E,I,Q,S,S,T,O,U,Y,N)
  (10,A,E,E,I,Q,S,S,O,T,U,Y,N)
  (10,A,E,E,I,Q,S,O,S,T,U,Y,N)
  (10,A,E,E,I,Q,O,S,S,T,U,Y,N)
  (11,A,E,E,I,O,Q,S,S,T,U,Y,N)
  (11,A,E,E,I,O,Q,S,S,T,U,N,Y)
  (11,A,E,E,I,O,Q,S,S,T,N,U,Y)
  (11,A,E,E,I,O,Q,S,S,N,T,U,Y)
  (11,A,E,E,I,O,Q,S,N,S,T,U,Y)
  (11,A,E,E,I,O,Q,N,S,S,T,U,Y)
  (11,A,E,E,I,O,N,Q,S,S,T,U,Y)*/
}

object N5 {
  //1 2 3 4 5 .. N
}

object N6 {
  //插入排序更快，因为其对于无法移动情况直接停止，而选择排序对输入不敏感
}

object N7 {
  //9 8 7 6 5 4 3 2 1
  //对于逆序不重复数组，选择排序消耗线性时间内循环，但是进行了 N^2 级别的比较
  //而插入排序则进行了 N 的线性内循环，之后对于每个值进行了最糟糕的移动方案，大概 0.25N^2 级别的移动和比较。
  //如果移动耗时最大，那么看起来当 0.25N^2 > N 的时候，即 N > 4 的时候，选择排序较快。
}

object N8 {
  //应该是介于两者之间的，因为只有线性的内循环，之后因为局部有序，因此其不会对于每个值进行大范围的插入，这导致了其绝对小于 0.25N^2 的耗时
  //因此应该介于两者之间
}

object N9 extends App {
  class ShellExp[T<:Comparable[T]] extends SortBasic[T] {
    def sort(a:Array[T]): Unit = {
      val aUsed = Array.fill(a.length)(0)
      val N = a.length
      val hs = (1 to N).foldLeft(Array(1)) {
        (sum, now) => if (now == sum.last * 3 + 1) sum :+ now else sum }
      hs.reverse.foreach { h =>
        (h until N).foreach { i =>
          (1 to i).reverse.foreach { j =>
            if (j - h >= 0) {
              aUsed(j) += 1
              aUsed(j - h) += 1
            }
            if ((j - h >= 0) && less(a(j), a(j - h))) {
              //println(h,i,j,a.mkString(","))
              exch(a, j, j - h)
            }
          }
        }
      }
      //println("aUsed",aUsed.mkString(", "))
      //println("aUsed/aLength",aUsed.map(i => (i * 1.0/a.length).formatted("%.3f")).mkString(","))
      println("Result",aUsed.map(i => i * 1.0/a.length).sum * 1.0/aUsed.length)
    }
  }
  new ShellExp[String] { }.sort(Array("E","A","S","Y","Q","U","E","S","T","I","O","N"))
  /*(4,6,6,E,A,S,Y,Q,U,E,S,T,I,O,N)
  (4,7,7,E,A,E,Y,Q,U,S,S,T,I,O,N)
  (4,9,9,E,A,E,S,Q,U,S,Y,T,I,O,N)
  (4,10,10,E,A,E,S,Q,I,S,Y,T,U,O,N)
  (4,11,11,E,A,E,S,Q,I,O,Y,T,U,S,N)
  (4,11,7,E,A,E,S,Q,I,O,N,T,U,S,Y)
  (1,1,1,E,A,E,N,Q,I,O,S,T,U,S,Y)
  (1,5,5,A,E,E,N,Q,I,O,S,T,U,S,Y)
  (1,5,4,A,E,E,N,I,Q,O,S,T,U,S,Y)
  (1,6,6,A,E,E,I,N,Q,O,S,T,U,S,Y)
  (1,10,10,A,E,E,I,N,O,Q,S,T,U,S,Y)
  (1,10,9,A,E,E,I,N,O,Q,S,T,S,U,Y)*/
  //因为插入排序对于 h有序的情况处理最好 - 选择排序对输入不敏感，而插入排序则对局部有序的情况适应非常好。
  def stream(start:Int=10):Stream[Int] = start #:: stream(Math.pow(10,Math.log10(start) + 1).toInt)
  val N = 6
  println(stream().take(N).mkString(","))
  val shell = new ShellExp[lang.Double] { }
  stream().take(N).foreach { N =>
    val array = Array.fill(N)(new lang.Double(Random.nextDouble()))
    Utils.ptime1 {
      shell.sort(array)
    }
  }
}