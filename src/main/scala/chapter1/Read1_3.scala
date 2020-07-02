package chapter1.Read1_3

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.reflect.ClassTag

/**
 * 只进不出的 Bag 数据结构
 * 多用于对一系列数据进行遍历或者统计的场合
 */
trait Bag[T] extends Iterable[T] {
  def add(t:T):Unit
  def isEmpty: Boolean
  def size:Int
}
/**
 * FIFO 先进先出的队列 数据结构
 * 日常生活最为常用
 */
trait Queue[T] extends Iterable[T] {
  def enqueue(t:T):Unit
  def dequeue():T
  def isEmpty: Boolean
  def size:Int
}
/**
 * LIFO 后进先出的堆栈/下压栈 数据结构
 * 大多数计算机系统都基于堆栈操作，比如阅读邮件、超链接访问、表达式计算
 */
trait Stack[T] extends Iterable[T] {
  def push(t:T):Unit
  def pop():T
  def isEmpty: Boolean
  def size:Int
}

object SimpleEvaluate extends App {
  class SimpleStack[T] extends Stack[T] {
    private val buffer = new ArrayBuffer[T]()
    override def push(t: T): Unit = buffer.prepend(t)
    override def pop(): T = buffer.remove(0)
    override def iterator: Iterator[T] = buffer.iterator
  }
  val ops = new SimpleStack[String]
  val eles = new SimpleStack[Double]
  def evaluate(in:String): Double = {
    //(1 * 3 + (3 * 4))
    in.split(" ").foreach {
      case "(" | " " =>
      case i@("+" | "-" | "*" | "/" | "sqrt") => ops.push(i.toString)
      case ")" =>
        val e = eles.pop()
        val res = ops.pop() match {
          case "+" => eles.pop() + e
          case "-" => eles.pop() - e
          case "/" => eles.pop() / e
          case "*" => eles.pop() * e
          case "sqrt" => Math.sqrt(e)
        }
        eles.push(res)
      case e => eles.push(e.toDouble);
    }
    eles.pop()
  }
  println(evaluate("( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )"))
  println(evaluate("( ( 1 + sqrt ( 5 ) ) / 2 )"))
  //缺点，基于空格拆分符号，对表达式写法有要求。且只能进行二元运算，三元及以上需要加括号
}

trait StackTest {
  trait TStack[M] {
    def push(t:M):Unit
    def pop():M
    def isEmpty:Boolean
    def size:Int
  }
  val stack: TStack[String]
  //to be or not to - be - - that - - - is
  StdIn.readLine("Input words>>> ").split(" ").map(_.trim).foreach {
    case "-" =>
      if (!stack.isEmpty) println("Pop ", stack.pop())
    case item =>
      println("Pushing ", item); stack.push(item)
  }
  println("(" + stack.size + " left on stack)")
}

object FixedCapacityStackOfStringsDemo extends App with StackTest {
  class FixedCapacityStackOfStrings(cap:Int) {
    private var a: Array[String] = new Array[String](cap)
    private var N: Int = 0
    def push(t: String): Unit = { if (N < cap) { N += 1; a(N) = t } }
    def pop(): String = { N -= 1; a(N+1) }
    def isEmpty:Boolean = N == 0
    def size:Int = N
  }
  lazy val stack = new FixedCapacityStackOfStrings(100) with TStack[String]
}

object FixedCapacityStackDemo extends App with StackTest {
  class FixedCapacityStack[T:ClassTag](cap:Int) {
    private var a: Array[T] = new Array[T](cap)
    private var N: Int = 0
    def push(t: T): Unit = { if (N < cap) { N += 1; a(N) = t}}
    def pop(): T = { N-=1; a(N+1) }
    def isEmpty:Boolean = N == 0
    def size:Int = N
  }
  lazy val stack = new FixedCapacityStack[String](100) with TStack[String]
}

/**
 * Scala T 泛型如果设置为 Null，则需要 Null Tag，此外，Array 泛型需要 ClassTag
 * Iterator 实现可复用父类的内部数据结构（数组）
 */
object FlexibleCapacityStackDemo extends App with StackTest {
  class FlexStack[T >: Null :ClassTag] extends Stack[T] {
    private var a: Array[T] = new Array[T](100)
    private var N: Int = 0
    def push(t: T): Unit = {
      N += 1; a(N-1) = t
      if (N == a.length) { resize(2*a.length) }
    }
    def pop(): T = {
      N-=1
      val item = a(N)
      a(N) = null
      if (N > 0 && N == a.length/4) resize(a.length/2)
      item
    }
    override def isEmpty:Boolean = N == 0
    override def size:Int = N
    private def resize(max:Int):Unit = {
      println("resizing from ", a.length,"to ",max)
      val temp = new Array[T](max)
      if (a.length < max) { //如果是扩张
        a.indices.foreach { i => temp(i) = a(i) }
        //如果是压缩
      } else temp.indices.foreach { i => temp(i) = a(i) }
      a = temp
    }
    override def iterator: Iterator[T] = new Iterator[T] {
      var n: Int = 0
      override def hasNext: Boolean = a(n) != null
      override def next(): T = { n += 1; a(n - 1) }
    }
  }
  lazy val stack = new FlexStack[String] with TStack[String]
  stack.iterator.foreach(println)
}

//上述为《算法》1.3.1 API 及其各自功能， 1.3.2 基于数组的实现： Bag、Queue、Stack 的实现（可伸缩，可迭代）
//1.3.3 基于链表的实现


