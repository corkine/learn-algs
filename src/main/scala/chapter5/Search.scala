package chapter5

import scala.collection.mutable

trait StringST[Value]:
  def put(key:String, value:Value): Unit
  def get(key:String): Value
  def delete(key:String): Unit
  def contains(key:String): Unit
  def isEmpty(): Boolean
  //返回符号表中和参数共有的最长的前缀匹配
  def longestPrefixOf(s:String): String
  //返回符号表中有参数开头的键
  def keyWithPrefix(s:String): Iterable[String]
  //返回符号表中所有和此字符串匹配的键，. 在参数中表示任何字符
  def keysThatMatch(s:String): Iterable[String]
  def size(): Int
  def keys(): Iterable[String]

object TrieST:
  private val R = 256
  private class Node:
    var that: Object = _
    val next: Array[Node] = Array.fill(R)(null)

class TrieST[Value] extends StringST[Value] {
  import TrieST._
  private var root: Node = _
  
  override def get(key: String): Value =
    val x = get(root, key, 0)
    if x == null then return null.asInstanceOf[Value]
    else x.that.asInstanceOf[Value]
  
  private def get(x:Node, key:String, d:Int): Node = 
    if x == null then return null
    if (d == key.length()) return x
    val c = key.charAt(d)
    get(x.next(c), key, d+1)

  override def put(key: String, value: Value): Unit =
    root = put(root, key, value, 0)
  
  private def put(x:Node, key:String, value:Value, d:Int): Node =
    var result: Node = x
    if (x == null) result = new Node()
    if d == key.length() then
      result.that = value.asInstanceOf[Object]
      return result
    end if
    val c = key.charAt(d)
    result.next(c) = put(result.next(c),key,value,d+1); result

  override def keysThatMatch(s: String): Iterable[String] =
    val q = new mutable.Queue[String]()
    collect(root,"",s,q); q
  
  private def collect(x:Node, pre:String, pat:String, q:mutable.Queue[String]): Unit =
    val d = pre.length
    if x == null then return
    if d == pat.length && x.that != null then q.enqueue(pre)
    if d == pat.length then return 
    val next = pat.charAt(d)
    (0 until R).foreach { c => 
      if next == '.' || next == c then collect(x.next(c),pre+c,pat,q)
    }
    
  override def longestPrefixOf(s: String): String =
    s.substring(0,search(root,s,0,0))
  
  private def search(x:Node, s:String, d:Int, leng:Int): Int = 
    var length = leng
    if x == null then return length
    if x.that != null then length = d
    if d == s.length then return length
    val c = s.charAt(d)
    search(x.next(c),s,d+1,length)

  override def delete(key: String): Unit = root = delete(root,key,0)
  
  private def delete(xx:Node, key:String, d:Int): Node =
    var x = xx
    if x == null then return null
    if d == key.length then
      x.that = null.asInstanceOf[Node] 
    else
      val c = key.charAt(d)
      x.next(c) = delete(x.next(c),key,d+1)
    if x.that != null then return x
    (0 until R).foreach { c => 
      if x.next(c) != null then return x
    }; return null
  
  override def size(): Int = size(root)
  
  private def size(x:Node): Int =
    if x == null then return 0
    var cnt = 0
    if x.that != null then cnt += 1
    (0 until R).foreach { c => cnt += size(x.next(c))}; 
    cnt
  
  override def keys(): Iterable[String] = keyWithPrefix("")

  override def keyWithPrefix(s: String): Iterable[String] =
    val q = new mutable.Queue[String]()
    collect(get(root, s, 0), s, q)
    return q
  
  private def collect(x:Node, pre:String, 
                      q:mutable.Queue[String]): Unit =
    if x == null then return 
    if x.that != null then q.enqueue(pre)
    (0 until R).foreach { c => collect(x.next(c), pre + c, q)}

  override def contains(key: String): Unit = ???
  override def isEmpty(): Boolean = ???
}

class TST[V] extends StringST[V] {
  private case class Node(
    c:Char, 
    var left:Node, 
    var right:Node, 
    var mid:Node, 
    var v:V = null.asInstanceOf[V])
  
  private var root: Node = _

  override def get(key: String): V = get(root,key,0).v
  
  private def get(x:Node, key:String, d:Int): Node =
    if x == null then return null
    val c = key.charAt(d)
    if c < x.c then return get(x.left, key, d)
    else if c > x.c then return get(x.right, key, d)
    else if d < key.length - 1 then return get(x.mid, key, d + 1)
    else return x

  override def put(key: String, value: V): Unit = put(root,key,value,0)
  
  private def put(xx:Node, key:String, v:V, d:Int): Node =
    var x = xx
    val c = key.charAt(d)
    if x == null then x = Node(c,null,null,null) 
    else if c < x.c then x.left = put(x.left,key,v,d)
    else if c > x.c then x.right = put(x.right,key,v,d)
    else if d < key.length - 1 then x.mid = put(x.mid,key,v,d+1)
    else x.v = v
    return x

  override def contains(key: String): Unit = ???
  override def size(): Int = ???
  override def longestPrefixOf(s: String): String = ???
  override def keyWithPrefix(s: String): Iterable[String] = ???
  override def keysThatMatch(s: String): Iterable[String] = ???
  override def keys(): Iterable[String] = ???
  override def isEmpty(): Boolean = ???
  override def delete(key: String): Unit = ???
}

def search(pat:String, txt:String):Int =
  val M = pat.length
  val N = txt.length
  @inline def compare(i:Int): Int =
    (0 until M).foreach { j =>
      if txt.charAt(i+j) != pat.charAt(j) then return j
    }; return M
  (0 to N - M).foreach { i =>
    if compare(i) == M then return i 
  }; return N

def search2(pat:String, txt:String):Int = {
  var i,j = 0
  val M = pat.length
  val N = txt.length
  while i < N && j < M do
    if txt.charAt(i) == pat.charAt(j) then j += 1
    else { i -= j; j = 0 }
    i += 1
  end while
  if j == M then i - M else N
}