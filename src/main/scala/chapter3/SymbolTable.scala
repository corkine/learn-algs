package chapter3

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.reflect.ClassTag

/**
 * SymbolTable 符号表，又称字典 Dict，索引
 * 键不能重复（复写equals方法），重复后自动覆盖，键不能为空，值不能为空
 * 延时删除一般会将 key 的 value 置为 null（默认），即时删除则不会。延时删除可能造成 ST 中在某一时刻键的值为空。
 *
 * @tparam K Key 键
 * @tparam V Value 值
 */
trait SymbolTable[K,V] {
  def put(key:K, value:V): Unit
  def get(key:K): V
  def delete(key:K): Unit
  def size: Int
  def keys: Iterable[K]
  def contains(key:K): Boolean = get(key) != null
  def isEmpty: Boolean = size == 0
  def apply(key: K): V = get(key)
  def update(key: K, value:V): Unit = put(key, value)
}

/**
 * 排序过的 SymbolTable，添加了一些更方便的方法，比如 floor、ceiling 获取小于等于或者大于等于 某个键类型值的最大和最小键，keys 获取上下界限中的键的集合，select 排名为 k 的键，rank 此键的排名，注意 i == rank(select(i)), key == select(rank(key))
 * @tparam K Key 键
 * @tparam V Value 值
 */
trait OrderedSymbolTable[K <:Comparable[K], V]
  extends SymbolTable[K,V] {
  def min: K
  def max: K
  def floor(value: K): K
  def ceiling(value: K): K
  def rank(key: K): Int
  def select(index: Int): K
  def keysFrom(lo: K, hi: K): Iterable[K]
  def size(lo: K, hi: K): Int =
    if (hi.compareTo(lo) < 0) 0
    else if (contains(hi)) rank(hi) - rank(lo) + 1
    else rank(hi) - rank(lo)
  def deleteMin(): Unit = delete(min)
  def deleteMax(): Unit = delete(max)
  override def keys: Iterable[K] = keysFrom(min,max)
}

object Test extends App {
  val source = Source.fromFile("data/tale.txt")
  val data = source.getLines().toArray.flatMap(_.split(" ").map(_.trim))
  tools.Utils.ptime3 {
    val resultData = new mutable.HashMap[String,Int]()
    val list = new ArrayBuffer[String]()
    data.foldLeft(resultData -> list) { case ((result, list), now) =>
      if (now.length >= 10) list.append(now)
      result.update(now, result.getOrElse(now,0) + 1); result -> list
    }
    println(resultData) //0.053s
    println(list.mkString(", "))
  }
  source.close()
}

/**
 * 基于链表的顺序查找的 SymbolTable
 * @tparam K Key 键
 * @tparam V Value 值
 */
class SequentialSearchSymbolTable[K,V >: Null] extends SymbolTable[K,V] {
  class Node(val key:K, var value:V, var next: Node) {
    def content: String =
      s"| $key,$value ${if (next != null) next.content else "| null"}"
  }
  private var first: Node = _
  override def put(key: K, value: V): Unit = {
    var now = first
    while (now != null) {
      if (key.equals(now.key)) {
        now.value = value; return //命中
      }
      now = now.next
    }
    first = new Node(key, value, first) //未命中
  }
  override def get(key: K): V = {
    var now = first
    while (now != null) {
      if (key.equals(now.key)) return now.value //命中
      now = now.next
    }
    null //未命中
  }

  override def delete(key: K): Unit = {
    //对于第一个元素的删除
    if (first != null && first.key.equals(key)) {
      first = first.next
    } else { //对于其余元素的删除
      var prev: Node = first
      var now = first.next
      while (now != null) {
        if (key.equals(now.key)) {
          prev.next = now.next
        }
        prev = now
        now = now.next
      }
    }
  }

  def elementPrint(): Unit = println(if (first != null) first.content else "| null ")

  override def size: Int = {
    var now = first
    var count = 0
    while (now != null) {
      count += 1
      now = now.next
    }
    count
  }

  override def keys: Iterable[K] = {
    val stack = new mutable.Stack[K]()
    var now = first
    while (now != null) {
      stack.push(now.key)
      now = now.next
    }
    stack
  }

  override def toString: String = s"[SequentialSearchST]@${hashCode()}"
}

object SSSTAPITest extends App {
  val st = new SequentialSearchSymbolTable[String,Integer]
  st.put("A",1)
  st.elementPrint()
  st.put("B",2)
  st.elementPrint()
  st.put("C",3)
  st.elementPrint()
  st.put("B",8)
  st.elementPrint()
  st.delete("C")
  st.elementPrint()
  println(st)
  st.keys.map(i => (i,st.get(i))).foreach(println)
}

object SSSTLargeFileTest extends App {
  val source = Source.fromFile("data/tale.txt")
  val data = source.getLines().toArray.flatMap(_.split(" ").map(_.trim))
  tools.Utils.ptime1 {
    val resultData = new SequentialSearchSymbolTable[String,Integer]
    data.foreach { word =>
      if (!resultData.contains(word)) resultData.put(word,1)
      else resultData.put(word,resultData.get(word) + 1)
    }
    resultData.keys.foreach(println) //19s 比 Scala HashMap 满了 358 倍
  }
  source.close()
}

class BinarySearchSymbolTable[K >:Null <:Comparable[K] :ClassTag, V >:Null :ClassTag](capacity: Int = 1)
  extends OrderedSymbolTable[K,V] {

  private var ks: Array[K] = new Array[K](capacity)
  private var vs: Array[V] = new Array[V](capacity)
  private var N: Int = 0

  def elementPrint(): Unit = {
    val sb = new mutable.StringBuilder()
    ks.indices.foreach { i =>
      if (ks(i) == null) { println(sb.toString()); return }
      sb.append(ks(i)).append("-").append(vs(i)).append(", ")
    }
    println(sb.toString())
  }

  private def resize(maxCap:Int): Unit = {
    //println(s"resize to $maxCap")
    val tempKs = new Array[K](maxCap)
    val tempVs = new Array[V](maxCap)
    if (maxCap > ks.length) {
      System.arraycopy(ks,0,tempKs,0,ks.length)
      System.arraycopy(vs,0,tempVs,0,vs.length)
    } else {
      System.arraycopy(ks,0,tempKs,0, maxCap)
      System.arraycopy(vs,0,tempVs,0, maxCap)
    }
    ks = tempKs; vs = tempVs
  }

  override def put(key: K, value: V): Unit = {
    if (N == ks.length) resize(N * 2)
    val i = rank(key)
    if (i < N && ks(i).compareTo(key) == 0) { //如果找到 key 就地更新
      vs(i) = value; return
    }
    ((i+1) to N).reverse.foreach { j => //否则从数组中腾出 i 索引的位置，插入 i，保持数组有序
      ks(j) = ks(j-1)
      vs(j) = vs(j-1)
    }
    ks(i) = key; vs(i) = value
    N += 1
  }

  override def get(key: K): V = {
    if (isEmpty) return null
    val i = rank(key)
    if (i < N && ks(i).compareTo(key) == 0) vs(i)
    else null
  }

  override def rank(key: K): Int = {
    var (lo,hi) = (0, N-1)
    while (lo <= hi) {
      val mid = lo + (hi - lo)/2
      val cmp = key.compareTo(ks(mid))
      if (cmp < 0) hi = mid - 1
      else if (cmp > 0) lo = mid + 1
      else return mid
    }
    lo
  }

  override def delete(key: K): Unit = {
    val i = rank(key)
    println(s"now i $i, N $N")
    if (i < N && ks(i).compareTo(key) == 0) {
      (i until (N-1)).foreach { j =>
        ks(j) = ks(j+1)
        vs(j) = vs(j+1)
      }
      ks(N-1) = null
      vs(N-1) = null
    }
    if (N == ks.length/2) resize(N / 2)
    N -= 1
  }

  override def size: Int = N

  override def min: K = ks(0)

  override def max: K = ks(N-1)

  override def select(index: Int): K = ks(index)

  //如果越界则直接报错
  override def floor(value: K): K = ks(rank(value) + 1)

  override def ceiling(value: K): K = ks(rank(value))

  override def keysFrom(lo: K, hi: K): Iterable[K] = {
    val queue = mutable.Queue.empty[K]
    val (llo, hhi) = (rank(lo),rank(hi))
    (llo until hhi).foreach { i => queue.enqueue(ks(i))}
    if (contains(hi)) queue.enqueue(ks(hhi))
    queue
  }

  override def toString: String = s"BinarySearchSymbolTable#${hashCode()}"
}

object BSSTAPITest extends App {
  val st = new BinarySearchSymbolTable[String,Integer]()
  st.put("A",1)
  st.elementPrint()
  st.put("B",2)
  st.elementPrint()
  st.put("C",3)
  st.elementPrint()
  st.put("B",8)
  st.elementPrint()
  st.delete("C")
  st.elementPrint()
  st.delete("B")

  st.elementPrint()
  println(st)
  st.keys.map(i => (i,st.get(i))).foreach(println)
}

object BSSTLargeFileTest extends App {
  val source = Source.fromFile("data/tale.txt")
  val data = source.getLines().toArray.flatMap(_.split(" ").map(_.trim))
  tools.Utils.ptime1 {
    val resultData = new BinarySearchSymbolTable[String,Integer]
    data.foreach { word =>
      if (!resultData.contains(word)) resultData.put(word,1)
      else resultData.put(word,resultData.get(word) + 1)
    }
    resultData.keys.foreach(println) //0.458s 比 Scala HashMap 慢了 8.6 倍
  }
  source.close()
}

class BinarySearchTreeSymbolTable[K >:Null <:Comparable[K], V >:Null]
  extends OrderedSymbolTable[K,V] {

  class Node(var key:K,
             var value:V,
             var N: Int,
             var left: Node = null,
             var right: Node = null) {
    override def toString: String = s"[Node]$key-$value($N)"
    def show: String = tools.Utils.withStringBuilder { sb =>
      sb.append("\t\t").append(key).append("-").append(value).append(s"($N)").append("\n")
      sb.append(if (left == null) "null" else left.show).append("\t\t\t\t")
        .append(if (right == null) "null" else right.show)
      sb.append("\n")
    }
  }

  private var root: Node = _

  private def print(x:Node): Unit = {
    if (x == null) return
    print(x.left)
    Predef.print(x.key + " ")
    print(x.right)
  }

  def print(): Unit = {
    print(root); println("")
  }

  private def size(node:Node) = if (node == null) 0 else node.N

  @scala.annotation.tailrec
  private def getRec(node: Node, key: K): V = {
    if (node == null) return null
    val cmp = key.compareTo(node.key)
    if (cmp < 0) getRec(node.left, key)
    else if (cmp > 0) getRec(node.right, key)
    else node.value
  }

  override def get(key: K): V = getFast(root,key)

  def getFast(root: Node, key: K): V = {
    var x = root
    while (x != null) {
      val cmp = key.compareTo(x.key)
      if (cmp == 0) return x.value
      else if (cmp < 0) x = x.left
      else if (cmp > 0) x = x.right
    }; null
  }

  private def put(node: Node, key: K, value: V): Node = {
    if (node == null) return new Node(key, value, 1)
    val cmp = key.compareTo(node.key)
    if (cmp < 0) node.left = put(node.left, key, value)
    else if (cmp > 0) node.right = put(node.right, key, value)
    else node.value = value
    node.N = size(node.left) + size(node.right) + 1
    node
  }

  override def put(key: K, value: V): Unit = root = put(root, key, value)

  @scala.annotation.tailrec
  private def min(node:Node): Node = node.left match {
    case null => node
    case _ => min(node.left)
  }

  override def min: K = min(root).key

  @scala.annotation.tailrec
  private def max(node:Node): Node = node.right match {
    case null => node
    case _ => max(node.right)
  }

  override def max: K = max(root).key

  private def floor(node:Node, key:K): Node = {
    if (node == null) return null
    val cmp = key.compareTo(node.key)
    if (cmp == 0) return node
    if (cmp < 0) return floor(node.left, key)
    val t = floor(node.right,key)
    if (t != null) t else node
  }

  override def floor(key: K): K = {
    val x = floor(root, key)
    if (x == null) null else x.key
  }

  private def ceiling(node:Node, key:K): Node = {
    if (node == null) return null
    val cmp = key.compareTo(node.key)
    if (cmp == 0) return node
    if (cmp > 0) return ceiling(node.right, key)
    val t = ceiling(node.left,key)
    if (t != null) t else node
  }

  override def ceiling(key: K): K = {
    val x = ceiling(root, key)
    if (x == null) null else x.key
  }

  private def rank(x:Node, key:K): Int = {
    if (x == null) return 0
    val cmp = key.compareTo(x.key)
    if (cmp < 0) rank(x.left, key)
    else if (cmp > 0) 1 + size(x.left) + rank(x.right, key)
    else size(x.left)
  }

  override def rank(key: K): Int = rank(root, key)

  @scala.annotation.tailrec
  private def select(x:Node, index:Int): Node = {
    if (x == null) return null
    val t = size(x.left)
    if (t > index) select(x.left, index)
    else if (t < index) select(x.right, index-t-1)
    else x
  }

  override def select(index: Int): K = select(root, index).key

  override def keysFrom(lo: K, hi: K): Iterable[K] = {
    val q = mutable.Queue.empty[K]
    keysFrom(root, q, lo, hi)
    q
  }

  private def keysFrom(x:Node, queue: mutable.Queue[K], lo: K, hi:K): Unit = {
    if (x == null) return
    val cmplo = lo.compareTo(x.key)
    val cmphi = hi.compareTo(x.key)
    if (cmplo < 0) keysFrom(x.left, queue, lo, hi)
    if (cmplo <= 0 && cmphi >= 0) queue.enqueue(x.key)
    if (cmphi > 0) keysFrom(x.right, queue, lo, hi)
  }

  override def keys: Iterable[K] = keysFrom(min,max)

  private def deleteMin(x: Node): Node = {
    if (x.left == null) return x.right
    x.left = deleteMin(x.left)
    x.N = size(x.left) + size(x.right) + 1
    x
  }

  override def deleteMin(): Unit = root = deleteMin(root)

  private def deleteMax(x: Node): Node = {
    if (x.right == null) return x.left
    x.right = deleteMax(x.right)
    x.N = size(x.left) + size(x.right) + 1
    x
  }

  override def deleteMax(): Unit = root = deleteMax(root)

  private def delete(node:Node, key:K): Node = {
    var x = node
    if (x == null) return null
    val cmp = key.compareTo(x.key)
    if (cmp > 0) delete(x.right, key)
    else if (cmp < 0) delete(x.left, key)
    else {
      if (x.right == null) return x.left
      if (x.left == null) return x.right
      val t = x
      x = min(t.right)
      x.right = deleteMin(t.right)
      x.left = t.left
    }
    x.N = size(x.left) + size(x.right) + 1
    x
  }

  override def delete(key: K): Unit = root = delete(root,key)

  override def size: Int = size(root)
}

object BSTSTAPITest extends App {
  val st = new BinarySearchTreeSymbolTable[String,Integer]()
  st.put("A",1)
  st.print()
  st.put("B",2)
  st.print()
  st.put("C",3)
  st.print()
  st.put("B",8)
  st.print()
  println(st.get("A"))
  println("min",st.min)
  println("max",st.max)
  st.delete("C")
  st.print()
  st.delete("B")
  st.print()
  st.keys.map(i => (i,st.get(i))).foreach(println)
}

object BSTSTLargeFileTest extends App {
  val source = Source.fromFile("data/tale.txt")
  val data = source.getLines().toArray.flatMap(_.split(" ").map(_.trim))
  tools.Utils.ptime1 {
    val resultData = new BinarySearchTreeSymbolTable[String,Integer]
    data.foreach { word =>
      if (!resultData.contains(word)) resultData.put(word,1)
      else resultData.put(word,resultData.get(word) + 1)
    }
    //resultData.keys.foreach(println) //0.101s 比 Scala HashMap 慢了 1.9 倍
  }
  source.close()
}

