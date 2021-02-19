package chapter4

import edu.princeton.cs.algs4.In

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Sorting

trait Graph {
  /**
   * 顶点数
   */
  def V:Int

  /**
   * 边数
   */
  def E:Int

  /**
   * 添加一条边 v - w
   */
  def addEdge(v:Int, w:Int):Unit

  /**
   * 和 v 相邻的所有顶点
   */
  def adj(V:Int):Iterable[Int]

  def degree(v:Int):Int

  override def toString: String = {
    var s = s"$V vertices, $E edges\n"
    (0 until V).foreach { e =>
      val ar = adj(e).toArray
      Sorting.quickSort(ar)
      s = s + s"$e: " + ar.reverse.mkString(" ") + "\n"
    }; s
  }
}

object Graph {
  /**
   * 计算 v 点的度数
   */
  def degree(G:Graph, v:Int): Int = G.adj(v).sum

  /**
   * 所有顶点的最大度数
   */
  def maxDegree(G:Graph): Int = (0 until G.V).foldLeft(0){ (max, v) =>
    val d = degree(G,v); if (d > max) d else max
  }

  /**
   * 所有顶点的平均度数
   */
  def avgDegree(G:Graph): Double = 2 * G.E / G.V

  /**
   * 自环的个数
   */
  def numberOfSelfLoops(G:Graph):Int = (0 until G.V).foldLeft(0) {  (count, v) =>
    var newCount = count
    G.adj(v).foreach(w => if (v == w) newCount += 1)
    newCount / 2
  }
}

/**
 * 对于图 G 和顶点 s 进行的搜索
 */
abstract class Search(G:Graph, s:Int) {
  /**
   * v 和 s 是连通的吗？
   */
  def marked(v:Int):Boolean

  /**
   * 与 s 相连的顶点总数
   */
  def count():Int
}

class HashTableSetGraph extends Graph {

  protected val map: mutable.HashMap[Int,mutable.HashSet[Int]] =
    new mutable.HashMap[Int,mutable.HashSet[Int]]()

  override def V: Int = map.keys.size

  override def E: Int = map.foldLeft(0)((c,s) => c + s._2.size) / 2

  override def addEdge(v: Int, w: Int): Unit = {
    map.put(v,{ val s = map.getOrElse(v,new mutable.HashSet[Int]()); s.add(w); s })
    map.put(w,{ val s = map.getOrElse(w,new mutable.HashSet[Int]()); s.add(v); s })
  }

  override def adj(V: Int): Iterable[Int] = map.getOrElse(V,Array[Int]())

  override def degree(v:Int):Int = map(v).size

}

object HashTableSetGraph extends App {
  val demo: HashTableSetGraph = {
    val in = new In("data/mediumG.txt")
    println("Number of Vertex is " + in.readInt())
    val e = in.readInt()
    val g = new HashTableSetGraph
    println("Number of Edges is " + e)
    (0 until e).foreach { _ =>
      val (a,b) = (in.readInt(),in.readInt())
      //println(s"Adding $a and $b now...")
      g.addEdge(a,b)
    }; g
  }
  println(demo)
  val TARGET = 245
  val ping: Search = new DepthFirstSearch(demo,TARGET)
  val d_search: Paths = new DepthFirstPaths(demo,TARGET)
  val b_search: Paths = new BreadFirstPaths(demo,TARGET)
  val sb_ping = new StringBuilder()
  val sb_d_search = new StringBuilder()
  val sb_b_search = new StringBuilder()
  (0 until demo.V).foreach { v =>
    sb_d_search.append("\n" + TARGET + " to " + v + ": ")
    sb_b_search.append("\n" + TARGET + " to " + v + ": ")
    if (ping.marked(v)) sb_ping.append(v + " ")
    if (d_search.hasPathTo(v)) {
      d_search.pathTo(v).foreach { p => sb_d_search.append("-> " + p) }
    } else { sb_d_search.append(" NOT FOUND") }
    if (b_search.hasPathTo(v)) {
      b_search.pathTo(v).foreach { p => sb_b_search.append("-> " + p) }
    } else { sb_b_search.append(" NOT FOUND") }
  }; sb_ping.append("\n"); sb_d_search.append("\n"); sb_b_search.append("\n")
  if (ping.count() != demo.V) sb_ping.append("NOT ")
  sb_ping.append("connected\n\n")
  println("PING: " + sb_ping.toString())
  println("DEPTH SEARCH: " + sb_d_search.toString())
  println("BOARD SEARCH: " + sb_b_search.toString())

  val cc = new BFSConnectionComponent(demo)
  println(s"For this graph, count group ${cc.count}")
  val map = new mutable.HashMap[Int,Set[Int]]()
  (0 until demo.V).foreach { i => val id = cc.id(i); map.put(id,map.getOrElse(id,Set[Int]()) ++ Set(i))}
  map.keys.foreach(k => println(s"$k - ${map(k).mkString(" ")}"))
}

class DepthFirstSearch(G:Graph, s:Int) extends Search(G,s) {
  val c_marked: Array[Boolean] = new Array[Boolean](G.V)
  var c_count: Int = 0

  dfs(G,s)

  private def dfs(G:Graph, v:Int): Unit = {
    c_marked(v) = true
    c_count += 1
    G.adj(v).foreach { w => if (!c_marked(w)) dfs(G,w) }
  }

  override def marked(w: Int): Boolean = c_marked(w)
  override def count(): Int = c_count
}

abstract class Paths(G:Graph, s:Int) {
  def hasPathTo(v:Int): Boolean
  def pathTo(v:Int): mutable.Iterable[Int]
}

class DepthFirstPaths(G:Graph, s:Int) extends Paths(G, s) {
  val c_marked: Array[Boolean] = new Array[Boolean](G.V)
  val c_cached: Array[Int] = Array.fill(G.V)(-1)

  dfs(G,s)

  private def dfs(G:Graph, v:Int): Unit = {
    c_marked(v) = true
    G.adj(v).foreach { w =>
      if (!c_marked(w)) {
        dfs(G,w); c_cached(w) = v
      }
    }
  }

  override def hasPathTo(v: Int): Boolean = c_marked(v)

  override def pathTo(v: Int): mutable.Iterable[Int] = {
    if (!hasPathTo(v)) return Array[Int]()
    val stack = new mutable.Stack[Int]
    stack.push(v)
    if (v == s) return stack
    @tailrec def loopAndPush(a:Int):Unit = {
      val t = c_cached(a)
      if (t != s) { stack.push(t); loopAndPush(t) }
    }
    loopAndPush(v); stack.push(s); stack
  }
}

class BreadFirstPaths(G:Graph, s:Int) extends Paths(G,s) {
  private val cache = Array.fill(G.V)(-1)
  private val marked = new Array[Boolean](G.V)
  private val queue = new mutable.Queue[Int]()

  bfs2()

  private def bfs1(): Unit = {
    queue.enqueue(s)
    marked(s) = true
    while (queue.nonEmpty) {
      val now = queue.dequeue()
      G.adj(now).foreach { neb =>
        if (!marked(neb)) {
          queue.enqueue(neb)
          cache(neb) = now
          marked(neb) = true
        }
      }
    }
  }

  private def bfs2(): Unit = {
    queue.enqueue(s)
    marked(s) = true
    while (queue.nonEmpty) {
      val now = queue.dequeue()
      G.adj(now).foreach { neb =>
        if (!marked(neb)) {
          queue.enqueue(neb)
          cache(neb) = now
          marked(neb) = true
        }
      }
    }
  }

  override def hasPathTo(v: Int): Boolean = marked(v)

  override def pathTo(v: Int): mutable.Iterable[Int] = {
    if (!hasPathTo(v)) return Array[Int]()
    val stack = new mutable.Stack[Int]
    stack.push(v)
    if (v == s) return stack
    @tailrec def loopAndPush(a:Int):Unit = {
      val t = cache(a)
      if (t != s) { stack.push(t); loopAndPush(t) }
    }
    loopAndPush(v); stack.push(s); stack
  }
}

abstract class ConnectionComponent(G:Graph) {
  def connect(v:Int, w:Int): Boolean //判断 v 和 w 节点是否连通
  def count: Int //返回一共的连通组
  def id(v:Int): Int //返回节点 v 的连通组
}

class BFSConnectionComponent(G:Graph) extends ConnectionComponent(G) {

  private val _marked = new Array[Boolean](G.V)
  private val _id = new Array[Int](G.V)
  private var _count: Int = 0

  (0 until G.V).foreach { s => if (!_marked(s)) { dfs(G,s); _count += 1} }
  //如果 marked 过，那么一定有组了，因此 count 不递增，也不进行递归标记，反之则创建新组

  private def dfs(G:Graph,s:Int): Unit = {
    _marked(s) = true; _id(s) = _count
    G.adj(s).foreach { w => if (!_marked(w)) dfs(G,w) }
    //此处判断 marked w 并非多余（如果 w marked 过，那么其邻居 s 也 marked 过），因为可能这个邻居是刚检查过的
    //无向图和树不同，其邻居并不指的是子节点，而是周围平等的节点，因此要排除刚检查过的。
  }

  override def connect(v: Int, w: Int): Boolean = _id(v) == _id(w)

  override def count: Int = _count

  override def id(v: Int): Int = _id(v)
}

trait Digraph {

  def V:Int

  def E:Int

  def addEdge(v:Int, w:Int):Unit

  def adj(V:Int):Iterable[Int]

  def degree(v:Int):Int

  def reverse: Digraph
}

class SimpleDigraph(val V: Int) extends Digraph with Graph {
  protected val map: mutable.Map[Int, mutable.HashSet[Int]] = {
    val t = new mutable.HashMap[Int, mutable.HashSet[Int]]()
    (0 until V).foreach(i => t.put(i, new mutable.HashSet[Int])); t
  }

  var E: Int = 0

  override def addEdge(v: Int, w: Int): Unit = {
    map(v).add(w); E += 1
  }

  override def adj(V: Int): Iterable[Int] = map.getOrElse(V,Array[Int]())

  override def degree(v:Int):Int = map(v).size

  override def reverse: Digraph = {
    val r = new SimpleDigraph(V)
    (0 until V).foreach { v =>
      adj(v).foreach { w => r.addEdge(w, v)}
    }; r
  }
}

object SimpleDigraph extends App {
  val demo: SimpleDigraph = {
    val in = new In("data/mediumG.txt")
    val v = in.readInt()
    val e = in.readInt()
    val g = new SimpleDigraph(v)
    println("Number of Vertex is " + v)
    println("Number of Edges is " + e)
    (0 until e).foreach { _ =>
      val (a,b) = (in.readInt(),in.readInt())
      //println(s"Adding $a and $b now...")
      g.addEdge(a,b)
    }; g
  }
  println(demo)
  val search = new DirectedDFS(demo,202)
  val sb1 = new StringBuilder()
  sb1.append(s"For 202: ")
  (0 until demo.V).foreach { v =>
    if (search.marked(v)) sb1.append(v).append(" - ")
  }; println(sb1.toString())

  val demo2 = new SimpleDigraph(13)
  val data = Array(
    (0,1),(0,5),
    (2,0),(2,3),
    (3,2),(3,5),
    (4,3),(4,2),
    (5,4),
    (6,0),(6,4),(6,9),
    (7,6),(7,8),
    (8,7),(8,9),
    (9,10),(9,11),
    (10,12),
    (11,4),(11,12),
    (12,9))
  data.foreach { case (a,b) => demo2.addEdge(a,b) }
  println("DEMO2 " + demo2)
  val sdc = new SimpleDirectedCycle(demo2)
  println("Has this Graph have cycle? " + sdc.hasCycle)
  println(sdc.cycle.mkString("-"))

  val demo3 = new SimpleDigraph(7)
  val data3 = Array(
    (0,5),(0,6),(0,1),
    (2,0),
    (2,3),
    (5,4))
  val demo4 = new SimpleDigraph(13)
  val data4 = Array(
    (0,1),(0,5),(0,6),
    (2,0),(2,3),
    (3,5),
    (5,4),
    (6,4),(6,9),
    (7,6),
    (8,7),
    (9,10),(9,11),
    (11,12))
  data4.foreach { case (a,b) => demo4.addEdge(a,b) }
  data3.foreach { case (a,b) => demo3.addEdge(a,b) }
  println("DEMO3 " + demo3)
  val st = new SimpleTopological(demo4)
  println("The topological order of this Graph? " + !st.isDAG + " \n" + st.order.mkString("-"))

  println("The Graph strong connect result: \n")
  val kscc = new KosarajuSCC(demo2)
  println(kscc.ids.mkString(", "))

}

class DirectedDFS private() {
  def this(G:Digraph, s:Int) = {
    this(); this.G = G; this.s = s
    dfs(G,s)
  }
  def this(G:Digraph, source:mutable.Iterable[Int]) = {
    this(); this.G = G; this.source = source;
    this.source.foreach { s => if (!m_marked(s)) dfs(G,s) }
  }

  var G: Digraph = _
  var s: Int = _
  var source: mutable.Iterable[Int] = _

  protected lazy val m_marked: Array[Boolean] = new Array[Boolean](G.V)

  protected def dfs(G:Digraph, v:Int): Unit = {
    m_marked(v) = true
    G.adj(v).foreach { w => if (!m_marked(w)) dfs(G,w) }
  }

  def marked(v:Int): Boolean = m_marked(v)
}

trait DirectedCycle {
  val G: Digraph
  def hasCycle: Boolean
  def cycle: mutable.Iterable[Int]
}

class SimpleDirectedCycle(val G:Digraph) extends DirectedCycle {
  private val marked = new Array[Boolean](G.V)
  private val edgeTo = Array.fill(G.V)(0)
  private val onStack = new Array[Boolean](G.V)
  val cycle: mutable.Stack[Int] = new mutable.Stack[Int]()

  (0 until G.V).foreach { v => if (!marked(v)) dfs(G,v); }

  private def dfs(G:Digraph, v:Int): Unit = {
    onStack(v) = true
    marked(v) = true
    G.adj(v).foreach { w =>
      if (hasCycle) return
      else if (!marked(w)) {
        edgeTo(w) = v
        dfs(G,w)
      } else if (onStack(w)) {
        var now: Int = v
        while (now != w) {
          cycle.push(now)
          now = edgeTo(now)
        }
        cycle.push(w)
        cycle.push(v)
      }
    }
    onStack(v) = false
  }

  override def hasCycle: Boolean = cycle.nonEmpty
}

trait Topological {
  val G: Digraph
  def isDAG: Boolean
  def order: mutable.Iterable[Int]
}

class SimpleTopological(val G: Digraph) extends Topological {
  private val marked = new Array[Boolean](G.V)
  val pre = new mutable.Queue[Int]()
  val post = new mutable.Queue[Int]()
  val reversePost = new mutable.Stack[Int]()

  if (!new SimpleDirectedCycle(G).hasCycle)  (0 until G.V).foreach { i => if (!marked(i)) dfs(G,i) }

  private def dfs(G:Digraph, v:Int): Unit = {
    pre.enqueue(v)
    marked(v) = true
    G.adj(v).foreach { w => if (!marked(w)) dfs(G,w) }
    post.enqueue(v)
    reversePost.push(v)
  }

  override def order: mutable.Iterable[Int] = reversePost
  override def isDAG: Boolean = order.isEmpty
}

trait StrongConnectionComponent {
  val G:Digraph //有向图
  def stronglyConnected(v:Int, w:Int): Boolean //v 和 w 强连通
  def count: Int //强连通分量数
  def id(v:Int): Int //当前节点所属强连通分量
}

class KosarajuSCC(val G: Digraph) extends StrongConnectionComponent {
  private val reverseG = G.reverse
  private val order_mark = new Array[Boolean](reverseG.V)
  private val order_result = new mutable.Stack[Int]()

  (0 until reverseG.V).foreach { i => if (!order_mark(i)) order_dfs(reverseG,i) }

  private def order_dfs(reverseG:Digraph, v:Int): Unit = {
    order_mark(v) = true
    reverseG.adj(v).foreach { w => if (!order_mark(w)) order_dfs(reverseG,w) }
    order_result.push(v)
  }

  private val scc_mark = new Array[Boolean](G.V)
  private val scc_id = new Array[Int](G.V)
  var count: Int = 0

  order_result.foreach { i => if (!scc_mark(i)) { scc_dfs(G,i); count += 1} }

  private def scc_dfs(G:Digraph, v:Int): Unit = {
    scc_mark(v) = true
    scc_id(v) = count
    G.adj(v).foreach { w => if (!scc_mark(w)) scc_dfs(G, w) }
  }

  override def stronglyConnected(v: Int, w: Int): Boolean = scc_id(v) == scc_id(w)

  override def id(v: Int): Int = scc_id(v)
  def ids = scc_id
}




