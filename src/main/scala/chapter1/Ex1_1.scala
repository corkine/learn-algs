package chapter1.Ex1_1
import tools.Utils._
import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

trait N13[T] {
  val data: Array[Array[T]]
  def print:String = withStringBuilder { sb =>
    data.foreach { line =>
      line.foreach { element =>
        sb.append(element.toString)
        sb.append(", ")
      }
      sb.append("\n")
    }
  }
  def printT:String = withStringBuilder { sb =>
    val row = data.length
    val column = data.head.length
    (1 to column).foreach { c =>
      (1 to row).foreach { r =>
        sb.append(data(r-1)(c-1))
        sb.append(", ")
      }
      sb.append("\n")
    }
  }
}

object N13Test extends App with N13[Double] {
  override val data = Array(Array(1D,2D,3D),Array(4D,5D,6D),Array(7D,8D,9D))
  println(print)
  println(printT)
}

object N14 extends App {
  /**
   * 这个实现不能很好的利用之前计算的值，每次都要重新计算 Math.pow(2,n)
   */
  def lg(n:Int):Int = {
    @inline def loop(start:Int): Stream[Int] = start #:: loop(start+1)
    loop(1).takeWhile(i => (1 to i).fold(1)((a,_) => 2 * a) < n).max
  }

  /**
   * 这个实现没有使用可变的变量，而是使用递归，每次缓存结果，下次直接使用
   */
  def lg2(n:Int):Int = {
    @scala.annotation.tailrec
    def loop(now:Int, value:Int):Int = {
      if (value > n) now - 1
      else loop(now + 1, value * 2)
    }
    loop(0,1)
  }
  println(lg(20))
  println(lg2(20))
}

object N15 extends App {
  def histogram(a:Array[Int], b:Int):Array[Int] = {
    @scala.annotation.tailrec
    def loop(i:Int, result:Array[Int]):Array[Int] = {
      if (i >= b) result
      else {
        val newArray = result.clone()
        newArray(i) += a.count(_ == i)
        loop(i + 1, newArray)
      }
    }
    loop(0,Array.fill(b)(0))
  }
  def histogram2(a:Array[Int], b:Int):Array[Int] = {
    val result = Array.fill(b)(0)
    for (i <- a) {
      if (i < b) {
        result(i) += 1
      }
    }
    result
  }
  def histogram3(a:Array[Int], b:Int):Array[Int] = {
    @scala.annotation.tailrec
    def loop(i:Int, result:Array[Int]):Array[Int] = {
      if (i >= a.length) result
      else {
        val newResult = result.clone()
        newResult(a(i)) += 1
        loop(i+1,newResult)
      }
    }
    loop(0,Array.fill(b)(0))
  }
  println(histogram(Array(1,2,3,4,3,3,2,2,1,4),13).mkString(","))
  println(histogram2(Array(1,2,3,4,3,3,2,2,1,4),13).mkString(","))
  println(histogram3(Array(1,2,3,4,3,3,2,2,1,4),13).mkString(","))
}

object N16 extends App {
  def exR1(n:Int):String = {
    if (n <= 0) return ""
    exR1(n-3) + n + exR1(n-2) + n
  }
  println(exR1(6))
}

object N18 extends App {
  //注意，b % 2 分支不写 return 结果将大相径庭
  def mystery(a:Int,b:Int):Int = {
    if (b == 0) return 0
    if (b % 2 == 0) return mystery(a+a, b/2)
    mystery(a+a, b/2) + a
  }
  println((mystery(2,25),mystery(3,11),2 * 25, 3 * 11))
  (1 to 20) foreach { i =>
    println((2,i,mystery(2,i)))
    println((3,i,mystery(3,i)))
  }
  def mystery2(a:Int,b:Int):Int = {
    if (b == 0) return 1
    if (b % 2 == 0) return mystery2(a*a, b/2)
    mystery2(a*a, b/2) * a
  }
  println((mystery2(2,25),mystery2(3,11),Math.pow(2,25),Math.pow(3,11)))
  (1 to 20) foreach { i =>
    println((2,i,mystery2(2,i)))
    println((3,i,mystery2(3,i)))
  }
}

object N19 extends App {
  def F(n:Int):Long = {
    if (n == 0) return 0
    if (n == 1) return 1
    F(n-1) + F(n-2)
  }
  def F2(n:Int):BigInt = {
    if (n == 0) return 0
    if (n == 1) return 1
    @scala.annotation.tailrec
    def loop(iter:Int, a:BigInt, b:BigInt):BigInt = {
      if (iter >= n) a + b
      else loop(iter + 1, b, a + b)
    }
    loop(2,0,1)
  }
  val start = System.currentTimeMillis()
  def ss(start:Int):Stream[Int] = start #:: ss(start + 1)
  ss(1).takeWhile { i => {
      println(i -> F(i))
      if (System.currentTimeMillis() - start < 1000 * 10) true
      else false
    }
  }.length
  println("End")
}

object N20 extends App {
  def lnNP(n:Int):Double = {
    @scala.annotation.tailrec
    def loop(n:Int, res:Long): Long = {
      if (n == 1) res * 1
      else loop(n-1, res * n)
    }
    println("loop" -> n -> loop(n,1))
    Math.log10(loop(n,1))
  }
//  println(lnNP(4))
}

object N21 extends App {
  case class Line(name:String,a:Int,b:Int) {
    def c:Double = a / b
  }
  def printTable(in:Array[Line]):String = withStringBuilder { sb =>
    in.foreach { line =>
      sb.append(" | ")
      sb.append("%-10s".format(line.name)).append(" | ")
        .append("%-10d".format(line.a)).append(" | ")
        .append("%-10d".format(line.b)).append(" | ")
        .append("%-10.3f".format(line.c)).append(" | ").append("\n")
      sb.append(" -----------------------------------------------------  \n")
    }
  }
  println(printTable(Array(Line("Corkine",2,3),Line("Marvin",3,4),Line("LiIi",3,1))))
  var reading = true
  val data = ArrayBuffer[Line]()
  while (reading) {
    val str = StdIn.readLine()
    if (str == "") reading = false
    else {
      val ss = str.split(" ")
      data.append(Line(ss(0),ss(1).toInt,ss(2).toInt))
    }
  }
  println(printTable(data.toArray))
}

object N22 extends App {
  def rank(a:Array[Int],b:Int):Int = {
    @scala.annotation.tailrec
    def loop(a:Array[Int], b:Int, from:Int, to:Int, level:Int):Int = {
      Thread.sleep(1000)
      println(" "*level + " " + "lo:%d hi:%d".format(from,to))
      if (from > to) -1
      else {
        val midIndex = (from + to)/2
        val mid = a(midIndex)
        if (b > mid) {
          println(" "*level + " " + "-" + mid)
          loop(a,b,midIndex + 1,to, level + 1)
        }
        else if (b < mid) {
          println(" "*level + " " + "-" + mid)
          loop(a,b,from,midIndex - 1, level + 1)
        }
        else {
          println(" "*level + " " + "+" + mid)
          midIndex
        }
      }
    }
    loop(a,b,0,a.length-1,1)
  }
  println(rank(Array(1,2,3,4,6,7,8,9,10,11,12),11))
}

object N24 extends App {
  def gcd(p:Int, q:Int):Int = {
    @scala.annotation.tailrec
    def loop(p:Int, q:Int, level:Int):Int = {
      println(" " * level + " p:%d,q:%d".format(p,q))
      if (q == 0) return p
      val r = p % q
      loop(q, r, level + 1)
    }
    loop(p,q,1)
  }
  println(gcd(105,24))
  println(gcd(1111111,1234567))
  def prove(): Unit = {
    (1 to Integer.MAX_VALUE).foreach { p =>
      (1 to Integer.MAX_VALUE).foreach { q =>
        gcd(p,q)
      }
    }
  }
  prove()
}

