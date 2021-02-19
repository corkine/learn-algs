package chapter1.Ex1_3

import java.time.{LocalDate, LocalDateTime}
import java.util

import chapter1.Read1_3.Node

import scala.collection.mutable
import scala.io.StdIn

object N2 extends App {
  val stack = new util.Stack[String]()
  stack.push("it")
  stack.push("was")
  stack.pop()
  stack.push("the")
  stack.push("best")
  stack.pop()
  stack.push("of")
  stack.push("times")
  stack.pop()
  stack.pop()
  stack.pop()
  stack.push("it")
  stack.push("was")
  stack.pop()
  stack.push("the")
  stack.pop()
  stack.pop()
  println("Result")
  stack.forEach(println)
}

object N3 extends App {
  //题目不清晰，如果是入栈必须从 0 开始压入，按照顺序，可随时停止，那么只有 a 可能产生
  //a - o
  //b - x
  //c - x
  //d - x
  //e - x
  //f - x
  //g - x
  //h - x
}

object N4 extends App {
  val right = "[()]{}{[()()]()}"
  val wrong = "[(])"
  val wrong2 = wrong + "}"
  val stack = mutable.Stack.empty[String]
  def check = { (in:String) =>
    var bad = false
    in.toCharArray.map(_.toString).foreach {
      case a@("[" | "(" | "{") => stack.push(a)
      case "]" => if (stack.nonEmpty && stack.pop() != "[") bad = true
      case ")" => if (stack.nonEmpty && stack.pop() != "(") bad = true
      case "}" => if (stack.nonEmpty && stack.pop() != "{") bad = true
    }
    val ans = stack.isEmpty && !bad
    stack.clear()
    ans
  }
  println((check(right),check(wrong),check(wrong2)))
}

object N5_6_7 extends App {
  def check = { (n:Int) =>
    var N = n
    val stack = new mutable.Stack[Int]()
    while (N > 0) {
      stack.push(N % 2)
      N = N / 2
    }
    stack.foreach(println)
  }
  check(50)

  def check2 = { (q: mutable.Queue[String]) =>
    val stack = new mutable.Stack[String]()
    while (q.nonEmpty) {
      stack.push(q.dequeue())
    }
    while (stack.nonEmpty) {
      q.enqueue(stack.pop())
    }
  }
  val queue = mutable.Queue.from(Array("Hello","World","Corkine","Ma"))
  check2(queue)
  //相当于给 Queue Reverse 了一遍
  println(queue)

  //Stack 的 peek 本质就是 pop 返回值，但不弹出，使用 List 实现时，即 Node#element
}

object N8 extends App {
  val stack = new mutable.Stack[String]()
  stack.push("it")
  stack.push("was")
  stack.pop()
  stack.push("the")
  stack.push("best")
  stack.pop()
  stack.push("of")
  stack.push("times")
  stack.pop()
  stack.pop()
  stack.pop()
  stack.push("it")
  stack.push("was")
  stack.pop()
  stack.push("the")
  stack.pop()
  stack.pop()
  println(stack)
}

object N9 extends App {
  def fix = { (in:String) =>
    val ops = mutable.Stack.empty[String]
    val eles = mutable.Stack.empty[String]
    in.toCharArray.map(_.toString.trim).filter(_.nonEmpty).foreach {
      case "(" =>
      case op@("+" | "-" | "*" | "/") => ops.push(op)
      case ")" =>
        val right = eles.pop()
        val op = ops.pop()
        val left = eles.pop()
        val result = "( %s %s %s )".format(left, op, right)
        eles.push(result)
      case num =>
        eles.push(num)
    }
    eles.fold("")(_ + _)
  }
  println(fix("1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )"))
}

object N10 extends App {
  def infixToPostfix = { (in:String) =>
    val ops = mutable.Stack.empty[String]
    val eles = mutable.Stack.empty[String]
    in.toCharArray.map(_.toString.trim).filter(_.nonEmpty).foreach {
      case "(" =>
      case op@("+" | "-" | "*" | "/") => ops.push(op)
      case ")" =>
        val right = eles.pop()
        val op = ops.pop()
        val left = eles.pop()
        val result = "( %s %s %s )".format(left, right, op)
        eles.push(result)
      case num =>
        eles.push(num)
    }
    eles.fold("")(_ + _)
  }
  println(infixToPostfix("1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )"))
}

object N11 extends App {
  def evaluatePostfix = { (in:String) =>
    val ops = mutable.Stack.empty[String]
    val eles = mutable.Stack.empty[String]
    in.toCharArray.map(_.toString.trim).filter(_.nonEmpty).foreach {
      case "(" =>
      case op@("+" | "-" | "*" | "/") => ops.push(op)
      case ")" =>
        val right = eles.pop()
        val left = eles.pop()
        val result = ops.pop() match {
          case "+" => left.toDouble + right.toDouble
          case "-" => left.toDouble - right.toDouble
          case "*" => left.toDouble * right.toDouble
          case "/" => left.toDouble / right.toDouble
        }
        eles.push(result.toString)
      case num =>
        eles.push(num)
    }
    eles.fold("")(_ + _)
  }
  println(evaluatePostfix(N10.infixToPostfix(N9.fix("1 + 2 ) * 3 - 4 ) * 5 - 6 ) ) )"))))
}

object N12 extends App {
  def copy(in:mutable.Stack[String]):mutable.Stack[String] = mutable.Stack.from(in.toSeq)
}

object N13 extends App {
  //a 可以
  //b 不可以
  //c 不可以
  //d 不可以
}

object N14 extends App {
  import chapter1.Read1_3.Queue
  class ResizingArrayQueueOfStrings extends Queue[String] {
    private var a: Array[String] = new Array[String](10)
    private var N: Int = 0
    private var lastIndex: Int = -1
    override def size: Int = N
    override def isEmpty: Boolean = N == 0
    override def enqueue(t: String): Unit = {
      println("enqueue" -> t)
      if (lastIndex + 1 == a.length - 1) resize(a.length * 2)
      a.indices.foreach(i => {
        val now = a(i)
        if (now != null) {
          lastIndex = i
        }
      })
      a(lastIndex + 1) = t
      N += 1
      println(a.mkString(","))
    }
    override def dequeue(): String = {
      println("dequeue")
      if (N == a.length/2) resize(a.length / 2)
      var ans = ""
      var finding = true
      a.indices.foreach(i => {
        val now = a(i)
        if (now != null && finding) {
          ans = now
          a(i) = null
          finding = false
        }
      })
      N -= 1
      println(a.mkString(","))
      ans
    }
    override def iterator: Iterator[String] = new Iterator[String] {
      //println(a.mkString(","))
      private var passed: Int = 0
      private val all: Int = a.count(_ != null)
      override def hasNext: Boolean = passed != all
      override def next(): String = {
        var nowCount = 0
        a.indices.foreach(i => {
          val ans = a(i)
          if (ans != null) {
            nowCount += 1
            if (nowCount > passed) {
              passed += 1
              return ans
            }
          }
        })
        ""
      }
    }
    private def resize(max:Int):Unit = {
      //println("Resizing from ", a.length, max)
      val temp = new Array[String](max)
      if (max > a.length) { //扩张
        a.indices.foreach(i => temp(i) = a(i))
      } else { //收缩
        temp.indices.foreach(i => temp(i) = a(i))
      }
      a = temp
    }
  }
  val queue = new ResizingArrayQueueOfStrings
  "HELLOWORLDHELLOWORLD".toCharArray.map(_.toString).zipWithIndex.foreach {
    case (a,b) =>
      if (b % 7 == 0 && b != 0) queue.dequeue()
      else queue.enqueue(a)
  }
  println("PRINTQUEUE")
  queue.iterator.foreach(println)
  /*queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()
  queue.dequeue()*/
}

object N15 extends App {
  val queue = mutable.Queue.empty[String]
  StdIn.readLine("Input Words by space").split(" ").map(_.trim).foreach { i =>
    queue.enqueue(i)
  }
  val key: Int = StdIn.readLine("Input you want find the last key index>> ").toInt
  val stack = mutable.Stack.from(queue.reverse)
  (1 until key).foreach { _ =>
    stack.pop()
  }
  println("It is" -> stack.pop())
}

object N16 extends App {
  var reading = true
  val queue = mutable.Queue.empty[String]
  while (reading) {
    val now = StdIn.readLine("Input a date>> ").trim
    if (now.isEmpty) reading = false
    else queue.enqueue(now)
  }
  println(queue.iterator.map(i => LocalDate.parse(i)).mkString(","))
}

object N19 extends App {
  /**
   * var now = first
   * while (now.next != null) {
   *    now = now.next()
   *    if (now != null && now.next() != null && now.next().next() == null) {
   *      now.next = null
   *    }
   * }
   */
}

object N20 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6
  def printList(first:Node[Int]):String = {
    var now = first
    val sb = new mutable.StringBuilder()
    while (now != null) {
      sb.append(now.element).append(", ")
      now = now.next
    }
    sb.toString()
  }
  def delete(index:Int, first:Node[Int]):Node[Int] = {
    var now = first
    var count = 0
    var needChangeNext = first
    var changedNext = first
    while (now != null) {
      if (count == index - 1) {
        needChangeNext = now
      } else if (count == index + 1) {
        changedNext = now
      }
      count += 1
      now = now.next
    }
    needChangeNext.next = changedNext
    first
  }
  println(printList(first))
  println(printList(delete(3,first)))
}

object N21 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6
  def find(list:Node[Int],key:Int):Boolean = {
    var now = list
    var finding = true
    while (now != null && finding) {
      println(now.element)
      if (now.element == key) finding = false
      now = now.next
    }
    !finding
  }
  println(find(first, 4))
}

object N24 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6
  def removeAfter(list:Node[Int],key:Node[Int]):Node[Int] = {
    var now = list
    var done = false
    while (now != null && !done) {
      if (now == key) {
        now.next = null
        done = true
      }
      now = now.next
    }
    list
  }
  def printList(first:Node[Int]):String = {
    var now = first
    val sb = new mutable.StringBuilder()
    while (now != null) {
      sb.append(now.element).append(", ")
      now = now.next
    }
    sb.toString()
  }
  println(printList(removeAfter(first, n5)))
}

object N25 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6

  val n100 = Node[Int]()
  n100.element = 100
  def insertAfter(list:Node[Int],key:Node[Int],inserted:Node[Int]):Node[Int] = {
    var now = list
    var done = false
    while (now != null && !done) {
      if (now == key) {
        inserted.next = now.next
        now.next = inserted
        done = true
      }
      now = now.next
    }
    list
  }
  def printList(first:Node[Int]):String = {
    var now = first
    val sb = new mutable.StringBuilder()
    while (now != null) {
      sb.append(now.element).append(", ")
      now = now.next
    }
    sb.toString()
  }
  println(printList(insertAfter(first, n5, n100)))
}

object N26 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 5
  n2.element = 2
  n3.element = 5
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6

  val n100 = Node[Int]()
  n100.element = 100
  def remove(list:Node[Int],key:Int):Node[Int] = {
    var fistBad = false
    var now = list
    while (now != null) {
      if (now.element == key) {
        fistBad = true
      }
      if (now.next != null) {
        val next = now.next
        if (next.element == key) {
          now.next = next.next
        }
      }
      now = now.next
    }
    if (!fistBad) list else list.next
  }
  def printList(first:Node[Int]):String = {
    var now = first
    val sb = new mutable.StringBuilder()
    while (now != null) {
      sb.append(now.element).append(", ")
      now = now.next
    }
    sb.toString()
  }
  println(printList(remove(first, 5)))
}

object N27 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6

  def max(list:Node[Int]):Int = {
    var now = list
    var max = 0
    while (now != null) {
      if (now.element > max) max = now.element
      now = now.next
    }
    max
  }
  println(max(first))
}

object N28 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 14
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6

  def max(list:Node[Int]):Int = {
    @scala.annotation.tailrec
    def loop(in:Node[Int], maxNow:Int):Int = {
      val max = if (in.element > maxNow) in.element else maxNow
      in.next match {
        case null => max
        case _ => loop(in.next, max)
      }
    }
    loop(list,0)
  }
  println(max(first))
}

object N29 extends App {
  import chapter1.Read1_3.Queue
  class CycleListQueue[T] extends Queue[T] {
    override def isEmpty: Boolean = false
    override def size: Int = 0
    var lastN: Node[T] = Node[T]()
    override def enqueue(t: T): Unit = {
      if (lastN.element == null) {
        lastN.element = t
      } else {
        val first = if (lastN.next == null) lastN else lastN.next
        val newLast = Node[T]()
        newLast.element = t
        newLast.next = first
        lastN.next = newLast
        lastN = newLast
      }
    }
    override def dequeue(): T = {
      val first = lastN.next
      val result = first.element
      lastN.next = first.next
      result
    }
    override def iterator: Iterator[T] = null
  }
  val queue = new CycleListQueue[String]
  queue.enqueue("H")
  queue.enqueue("E")
  queue.enqueue("L")
  queue.enqueue("L")
  queue.enqueue("O")
  queue.enqueue("M")
  queue.dequeue()
  queue.enqueue("Z")
  queue.dequeue()
  def printList(first:Node[String]):String = {
    var now = first
    var running = false
    val sb = new mutable.StringBuilder()
    while ((now != first && now != null)|| !running) {
      if (!running) running = true
      sb.append(now.element).append(", ")
      now = now.next
    }
    sb.toString()
  }
  println(printList(queue.lastN))
}

object N30 extends App {
  val first = Node[Int]()
  val n2 = Node[Int]()
  val n3 = Node[Int]()
  val n4 = Node[Int]()
  val n5 = Node[Int]()
  val n6 = Node[Int]()
  first.element = 1
  n2.element = 2
  n3.element = 3
  n4.element = 4
  n5.element = 5
  n6.element = 6
  first.next = n2
  n2.next = n3
  n3.next = n4
  n4.next = n5
  n5.next = n6
  def reverse(first:Node[Int]):Node[Int] = {
    if (first == null) return null
    if (first.next == null) return null
    val next = first.next
    val reversed = reverse(next)
    next.next = first
    first.next = null
    reversed
  }
  def printList(first:Node[Int]):String = {
    var now = first
    val sb = new mutable.StringBuilder()
    while (now != null) {
      sb.append(now.element).append(", ")
      now = now.next
    }
    sb.toString()
  }
  println(printList(reverse(n2)))
}

object N31 extends App {
  trait DoubleNode[T] {
    var element: T
    var head: DoubleNode[T]
    var next: DoubleNode[T]
  }
  object DoubleNode {
    def apply[T](): DoubleNode[T] = new DoubleNode[T]() {
      var element: T = _
      var head: DoubleNode[T] = _
      var next: DoubleNode[T] = _
    }
  }
  val first = DoubleNode[Int]()
  first.element = 1
  val second = DoubleNode[Int]()
  second.element = 2
  val third = DoubleNode[Int]()
  third.element = 3
  first.next = second
  second.next = third
  second.head = first
  third.head = second
  def insertFirst(n:DoubleNode[Int],ele:Int):DoubleNode[Int] = {
    val newNode = DoubleNode[Int]()
    n.head = newNode
    newNode.element = ele
    newNode.next = n
    newNode
  }
  def insertLast(n:DoubleNode[Int],ele:Int):DoubleNode[Int] = {
    @scala.annotation.tailrec
    def loop(n:DoubleNode[Int]):DoubleNode[Int] = n match {
      case i if i.next == null => i
      case o => loop(o.next)
    }
    val last = loop(n)
    val newNode = DoubleNode[Int]()
    newNode.head = last
    newNode.element = ele
    last.next = newNode
    n
  }
  def deleteFirst(n:DoubleNode[Int]):DoubleNode[Int] = {
    val next = n.next
    n.next = null
    next.head = null
    next
  }
  def deleteLast(n:DoubleNode[Int]):DoubleNode[Int] = {
    @scala.annotation.tailrec
    def loop(n:DoubleNode[Int]):DoubleNode[Int] = n match {
      case i if i.next == null => i
      case o => loop(o.next)
    }
    val last = loop(n)
    val secondLast = last.head
    secondLast.next = null
    last.head = null
    secondLast
  }
  def insertBefore(in:DoubleNode[Int],ele:Int):DoubleNode[Int] = {
    val newNode = DoubleNode[Int]()
    newNode.element = ele
    newNode.next = in
    newNode.head = in.head
    if (in.head != null) in.head.next = newNode
    newNode
  }
  def insertAfter(in:DoubleNode[Int],ele:Int):DoubleNode[Int] = {
    val newNode = DoubleNode[Int]()
    newNode.element = ele
    newNode.head = in
    newNode.next = in.next
    in.next.head = newNode
    in.next = newNode
    in
  }
  def deleteNode(node:DoubleNode[Int]):DoubleNode[Int] = {
    if (node.head != null && node.next != null) node.head.next = node.next.head
    else if (node.head != null) node.head.next = null
    else if (node.next != null) node.next.head = null
    node.head = null
    node.next = null
    node
  }

  @scala.annotation.tailrec
  def printNode(in:DoubleNode[Int]):Unit = {
    in match {
      case i if i == null => println("================")
      case o =>
        println("[NODE]" -> o.element)
        printNode(o.next)
    }
  }
//  printNode(first)
//  printNode(insertFirst(first,20))
//  printNode(insertLast(first,20))
//  printNode(deleteFirst(first))
//  printNode(deleteLast(first))
  printNode(deleteNode(first))
}