package chapter2.Ex2_4

import edu.princeton.cs.algs4.Insertion

import scala.collection.mutable
import scala.util.Random

object N1 extends App {
  implicit val ord: Ordering[String] = Ordering.String
  val pq = new mutable.PriorityQueue[String]()
  "P R I O * R * * I * T * Y * * * Q U E * * * U * E".split(" ").map(_.trim).foreach {
    case "*" => println("dequeue",pq.dequeue())
    case o => pq.enqueue(o)
  }
}

object N2 extends App {
  //每次插入数据都需要找到最大元素，其耗时为 O(N)，插入 N 个元素，则耗时为 O(N2)，此外还要额外维护一个栈/队列
  //如果直接进行 Sorting，最快的 QuickSort 或者 MergeSort 能够做到接近线性的 NlogN 水平，且无需额外维护一个栈/队列
}

object N3 extends App {
  //参考 Sort.scala 文件，有基于数组的实现
}

object N4 extends App {
  //对，其是一个满足堆定义的更特殊的情况
}

object N5 extends App {
  val pq = mutable.PriorityQueue.empty[String](Ordering.String)
  "EASYQUESTION".foreach { i => pq.enqueue(i.toString) }
  pq.clone().dequeueAll.foreach(println)
}

object N6 extends App {
  val pq = mutable.PriorityQueue.empty[String](Ordering.String)
  "PRIO*R**I*T*Y***QYE***Y*E".foreach {
    case '*' => pq.dequeue(); println(pq.clone().dequeueAll)
    case o => pq.enqueue(o.toString); println(pq.clone().dequeueAll)
  }
}

object N7 extends App {
  //需要满足条件：数字不重复且父大于子
  //k=1, 1
  //k=2, 2,3
  //k=3, 2,3,4,5,6,7
  //k=4, 4,5,6,7,8,9,10,11,12,13,14,15
}

object N8 extends App {
  //1 2-3 45-67 89-1011-1213-1415 1617-1819-2021-2223-2424-2627-2829-3031
  //k=1, 31,30,29,28..16
  //k=2, 31,..16
  //k=3, 31,..16
  //k=4, 31,..16
}

object N9 extends App {
  //MinPQ, MaxPQ
}

object N10 extends App {
  //0 1,2 3,4,5,6 7,8
  //(k-1)%2, 2k+1,2k+2
}

object N11 extends App {
  //无序数组插入最少，加入插入无限大，删除无限小，那么其时间复杂度接近 O(1)
}

object N12 extends App {
  //使用有序数组，查找最大元素为 O(1)
}

object N13_18 extends App {
  //j<N 检查是为了防止出现孤儿，即一个节点只包含一个子节点，这种情况出现在末尾，因此也可以对是否在末尾进行判断
  //1+log2N
  //从头向尾遍历，检查每个元素比父元素位置的值小，比子元素位置的值大
  //改变初始化堆有序时的定界位置，最少比较忽略所有没有子节点的节点（从N/2开始），最多比较则添加这些节点完全进行比较（从N开始）
  //略
  //插入元素上浮不影响，但删除最大时是将最底部元素和最大交换然后删除最大，因此其会每次进行子元素比较，将较大子元素放在上面，这个过程很大概率会打破原来数组，虽然还是堆有序的。如果整理过一次后，下一次将不会改变结果。
}

object N19_20 extends App {
  //每次添加元素后使用 swim 上游构造堆
  //下沉的构造堆每次添加元素使用 sink，将小元素放到底部（如果是 MaxPQ），sink 需要更多的比较（和两个子均进行比较）
}

object NN extends App {
  val a = Random.shuffle(Seq("a1","a2","b1","b2","c1","c2","c3","c4","d1","d2","e1","f1","f2")).toArray
  println(a.mkString(", "))
  Insertion.sort(a.asInstanceOf[Array[Object]], (x: String, y: String) => {
    //println(s"comp with $x and $y")
    val x1 = x.charAt(0).toString
    val y1 = y.charAt(0).toString
    val x2 = x.charAt(1).toInt
    val y2 = y.charAt(1).toInt
    val res = x1.compare(y1)
    val ans = if (res != 0) res else x2.compare(y2)
    //println(s"ans is $ans")
    ans
  })
  println(a.mkString(", "))
  val ele = a.sortBy(i => i.charAt(1))
  println(ele.mkString(", "))
}




