package chapter4

import edu.princeton.cs.algs4.In

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Sorting

abstract class Graph(val _v:Int) {
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

class HashTableSetGraph extends Graph(-1) {

  private val map: mutable.HashMap[Int,mutable.HashSet[Int]] =
    new mutable.HashMap[Int,mutable.HashSet[Int]]()

  override def V: Int = map.keys.size

  override def E: Int = map.foldLeft(0)((c,s) => c + s._2.size) / 2

  override def addEdge(v: Int, w: Int): Unit = {
    map.put(v,{ val s = map.getOrElse(v,new mutable.HashSet[Int]()); s.add(w); s })
    map.put(w,{ val s = map.getOrElse(w,new mutable.HashSet[Int]()); s.add(v); s })
  }

  override def adj(V: Int): Iterable[Int] = map(V)

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
  val search: Paths = new DepthFirstPaths(demo,TARGET)
  val sb_ping = new StringBuilder()
  val sb_search = new StringBuilder()
  (0 until demo.V).foreach { v =>
    sb_search.append("\n" + TARGET + " to " + v + ": ")
    if (ping.marked(v)) sb_ping.append(v + " ")
    if (search.hasPathTo(v)) {
      search.pathTo(v).foreach { p => sb_search.append("-> " + p) }
    } else { sb_search.append(" NOT FOUND") }
  }; sb_ping.append("\n"); sb_search.append("\n")
  if (ping.count() != demo.V) sb_ping.append("NOT ")
  sb_ping.append("connected\n\n")
  println("PING: " + sb_ping.toString())
  println("SEARCH: " + sb_search.toString())

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

