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
    override def isEmpty: Boolean = false
    override def size: Int = 0
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
      if (!stack.isEmpty) println("Pop " -> stack.pop())
    case item =>
      println("Pushing " -> item); stack.push(item)
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
    def isFull:Boolean = N == cap
    def size:Int = N
  }
  val stack = new FixedCapacityStackOfStrings(100) with TStack[String]
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
  val stack = new FixedCapacityStack[String](100) with TStack[String]
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
      println(("resizing from ", a.length,"to ",max))
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
  val stack = new FlexStack[String] with TStack[String]
  //stack.iterator.foreach(println)
}

//上述为《算法》1.3.1 API 及其各自功能， 1.3.2 基于数组的实现： Bag、Queue、Stack 的实现（可伸缩，可迭代）
//1.3.3 基于链表的实现

trait Node[T] {
  var element: T
  var next: Node[T]
}
object Node {
  def apply[T](): Node[T] = new Node[T]() {
    var element: T = _
    var next: Node[T] = _
  }
}
object NodeTest extends App {
  var first = Node[String]()
  val second = Node[String]()
  val third = Node[String]()
  first.element = "to"
  second.element = "be"
  third.element = "or"
  first.next = second
  second.next = third
  //添加头节点
  val oldFirst = first
  first = Node[String]()
  first.element = "not"
  first.next = oldFirst
  //删除头节点
  first = oldFirst
  //添加尾节点
  var last = Node[String]()
  last.element = "not"
  third.next = last
  //遍历
  var now = first
  while (now != null) {
    println(now)
    now = now.next
  }
}

object StackByListDemo extends App with StackTest {
  class ListStack[T] extends Stack[T] {
    private var first: Node[T] = _
    private var N: Int = _
    override def isEmpty: Boolean = N == 0
    override def size: Int = N
    def peek:T = first.element
    override def push(t: T): Unit = {
      val oldFirst = first
      first = Node[T]()
      first.element = t
      first.next = oldFirst
      N += 1
    }
    override def pop(): T = {
      val firstElement = first.element
      first = first.next
      N -= 1
      firstElement
    }
    override def iterator: Iterator[T] = new Iterator[T] {
      var now: Node[T] = first
      override def hasNext: Boolean = now == null
      override def next(): T = {
        val element = now.element
        now = now.next
        element
      }
    }
  }

  val stack = new ListStack[String] with TStack[String]
}

trait QueueTest {
  trait TQueue[M] {
    def enqueue(m:M):Unit
    def dequeue():M
    def isEmpty:Boolean
    def size:Int
  }
  val queue: TQueue[String]
  //to be or not to - be - - that - - - is
  StdIn.readLine("Input words>>> ").split(" ").map(_.trim).foreach {
    case "-" =>
      if (!queue.isEmpty) println("Dequeue " -> queue.dequeue())
    case item =>
      println("Enqueue " -> item); queue.enqueue(item)
  }
  println("(" + queue.size + " left on queue)")
}

object QueueByListDemo extends App with QueueTest {
  class ListQueue[T] extends Queue[T] {
    private var first: Node[T] = _
    private var N: Int = 0
    override def enqueue(t: T): Unit = {
      if (first == null) {
        first = Node[T]()
        first.element = t
      } else {
        var next: Node[T] = first
        while (next.next != null) {
          next = next.next
        }
        val last = Node[T]()
        last.element = t
        next.next = last
      }
      N += 1
    }
    override def dequeue(): T = {
      val firstElement = first.element
      first = first.next
      N -= 1
      firstElement
    }
    override def isEmpty: Boolean = N == 0
    override def size: Int = N
    override def iterator: Iterator[T] = new Iterator[T] {
      private var point: Node[T] = first
      override def hasNext: Boolean = point == null
      override def next(): T = {
        val element = point.element
        point = point.next
        element
      }
    }
  }

  val queue = new ListQueue[String] with TQueue[String]
}

trait BagTest {
  trait TBag[M] {
    def add(m:M):Unit
    def isEmpty:Boolean
    def size:Int
  }
  val bag: TBag[String]
  //to be or not to - be - - that - - - is
  StdIn.readLine("Input words>>> ").split(" ").map(_.trim).foreach {
    case "-" =>
      //if (!bag.isEmpty) println("Dequeue ", bag.add())
    case item =>
      println("Add " -> item); bag.add(item)
  }
  println("(" + bag.size + " left on bag)")
}

object BagByListDemo extends App with BagTest {
  class ListBag[T] extends Bag[T] {
    private var first: Node[T] = _
    private var N: Int = 0
    override def add(t: T): Unit = {
      if (first == null) {
        first = Node[T]()
        first.element = t
      } else {
        var next: Node[T] = first
        while (next.next != null) {
          next = next.next
        }
        val last = Node[T]()
        last.element = t
        next.next = last
      }
      N += 1
    }
    override def isEmpty: Boolean = N == 0
    override def size: Int = N
    override def iterator: Iterator[T] = new Iterator[T] {
      private var point: Node[T] = first
      override def hasNext: Boolean = point == null
      override def next(): T = {
        val element = point.element
        point = point.next
        element
      }
    }
  }

  val bag = new ListBag[String] with TBag[String]
}

//通过 Array 和 List 实现了 Bag、Queue、Stack 这三种数据结构，在后期
//基于这些基本的数据结构实现较为复杂的数据结构，并且通过算法在其中操纵这些
//数据结构以实现 API。这些数据结构包括父链接树、二分查找树、字符串、二叉堆、散列表、邻接链表、
//单词查找树、三项单词查找树等。程序是由算法 + 数据结构捏合而成的，前者代表了动作，后者
//代表了内部表现，其组合在一起共同实现了 Java 类的接口，以高效的满足应用的需求。












