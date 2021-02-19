package chapter4
import language.implicitConversions

import edu.princeton.cs.algs4
import edu.princeton.cs.algs4.{In, IndexMinPQ}

import java.lang
import scala.collection.convert.ImplicitConversions.`iterable AsScalaIterable`
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class DirectedEdge(from:Int,
                        to:Int,
                        weight: Double) {
  override def toString: String = s"DirectedEdge $from -> $to ($weight)"
}

class EdgeWeightedDigraph private() {
  private var bags: Array[ArrayBuffer[DirectedEdge]] = _
  private var v: Int = _
  private var e: Int = _
  def this(v:Int) = {
    this()
    this.v = v
    bags = (0 until v).map(_ => 
          new ArrayBuffer[DirectedEdge]()).toArray
  }
  def this(in:In) = {
    this()
    v = in.readLine().toInt
    e = in.readLine().toInt
    bags = (0 until v).map(_ => 
          new ArrayBuffer[DirectedEdge]()).toArray
    while (in.hasNextLine) {
      val line = in.readLine().split(" ")
      val edge = DirectedEdge(line(0).toInt,
                              line(1).toInt,line(2).toDouble)
      addEdge(edge)
    }
    in.close()
  }
  def V(): Int = v
  def E(): Int = e
  def addEdge(edge:DirectedEdge): Unit =  
    { bags(edge.from).addOne(edge); e += 1}
  def adj(node:Int):Iterable[DirectedEdge] = bags(node)
  def edges():Iterable[DirectedEdge] = 
    bags.foldLeft(Seq[DirectedEdge]())((sum,curr) => sum ++ curr)
  override def toString: String = 
    s"EdgeWeightedDigraph Point ${V()}, Edge ${E()}"
}

object EdgeWeightedDigraph {
  def test(getSp: (EdgeWeightedDigraph, Int) => SP,
           start: Int,
           data:String = "data/tinyEWD.txt"): Unit = {
    val G = new EdgeWeightedDigraph(new In(data))
    val sp = getSp(G,start)
    (0 until G.V()).foreach { t =>
      println(s"$start to $t: \n ${sp.distTo(t)}")
      if (sp.hasPathTo(t)) sp.pathTo(t).foreach { e => print(e.toString + " ")}
      println("")
    }
  }

  def main(args: Array[String]): Unit = {
    test((e,s) => new SP {
      override val G: EdgeWeightedDigraph = e
      override val start: Int = 3
      override def distTo(v: Int): Double = ???
      override def hasPathTo(v: Int): Boolean = ???
      override def pathTo(v: Int): Iterable[DirectedEdge] = ???
    },2)
  }
}

trait SP {
  val G: EdgeWeightedDigraph
  val start: Int
  def distTo(v:Int): Double
  def hasPathTo(v:Int): Boolean
  def pathTo(v:Int): Iterable[DirectedEdge]
}

class SimpleSP(override val G:EdgeWeightedDigraph,
               override val start:Int) extends SP {
  //从 start 到某索引节点的距离
  protected val dist: Array[Double] = {
    val temp = Array.fill(G.V())(Double.PositiveInfinity)
    temp(start) = 0; temp
  }
  //当前索引节点和其父节点的边
  protected val edge: Array[DirectedEdge] = Array.fill(G.V())(null)
  //边的放松：像皮筋一样，如果从 v -> w 这条边上，目前已知的到达 w 的路径大于
  //从 v 经由此边到 w 的距离，则更新到 w 的距离以经过 v 点和此边
  def relax(e: DirectedEdge): Unit = {
    val (v,w) = (e.from,e.to)
    if (dist(w) > dist(v) + e.weight) {
      dist(w) = dist(v) + e.weight
    }
  }
  //顶点的放松：一个顶点所有的边的放松
  def relax(v: Int, op: Int => Unit): Unit = {
    G.adj(v).foreach { e =>
      val w = e.to
      if (dist(w) > dist(v) + e.weight) {
        dist(w) = dist(v) + e.weight
        edge(w) = e
        op(v)
      }
    }
  }
  override def distTo(v: Int): Double = dist(v)
  override def hasPathTo(v: Int): Boolean = dist(v) < Double.PositiveInfinity
  override def pathTo(v: Int): Iterable[DirectedEdge] = {
    if (!hasPathTo(v)) return null
    val stack = new mutable.Stack[DirectedEdge]()
    var e = edge(v)
    while (e != null) {
      stack.push(e)
      e = edge(e.from)
    }; stack
  }
}

class DijkstraSP(g:EdgeWeightedDigraph, s:Int) extends SimpleSP(g,s) with SP {
  private val pq = new IndexMinPQ[lang.Double](G.V())
  pq.insert(s,0)
  while (!pq.isEmpty) relax(pq.delMin(), { w =>
    if (pq.contains(w)) pq.changeKey(w, dist(w))
    else pq.insert(w, dist(w))
  })
}

class DijkstraAllParisSP(G:EdgeWeightedDigraph) {
  private val all: Array[DijkstraSP] = (0 until G.V()).map(i => new DijkstraSP(G, i)).toArray
  def path(s:Int, t:Int): Iterable[DirectedEdge] = all(s).pathTo(t)
  def dist(s:Int, t:Int): Double = all(s).distTo(t)
}

class EWDAdaptor private(v:Int) extends algs4.EdgeWeightedDigraph(v:Int) {
  def this(g:EdgeWeightedDigraph) = {
    this(g.V()); g.edges()
      .foreach(sa => addEdge(new algs4.DirectedEdge(sa.from,sa.to,sa.weight)))
  }
}

class AcyclicSP(g:EdgeWeightedDigraph, s:Int) extends SimpleSP(g,s) with SP {
  new algs4.Topological(new EWDAdaptor(g))
    .order().forEach { v => relax(v,null) }
}

import edu.princeton.cs.algs4.AcyclicLP
import edu.princeton.cs.algs4.StdIn
import edu.princeton.cs.algs4.StdOut

/*
object CPM {
  def main(args: Array[String]): Unit = {
    val N = StdIn.readInt
    StdIn.readLine
    val G = new algs4.EdgeWeightedDigraph(2 * N + 2)
    val s = 2 * N
    val t = 2 * N + 1
    for (i <- 0 until N) {
      val a = StdIn.readLine.split("\\s+")
      val duration = a(0).toDouble
      G.addEdge(new Nothing(i, i + N, duration))
      G.addEdge(new Nothing(s, i, 0.0))
      G.addEdge(new Nothing(i + N, t, 0.0))
      for (j <- 1 until a.length) {
        val successor = a(j).toInt
        G.addEdge(new Nothing(i + N, successor, 0.0))
      }
    }
    val lp = new AcyclicLP(G, s)
    StdOut.println("Start times:")
    for (i <- 0 until N) {
      StdOut.printf("%4d: %5.1f\n", i, lp.distTo(i))
    }
    StdOut.printf("Finish time: %5.1f\n", lp.distTo(t))
  }
}*/
