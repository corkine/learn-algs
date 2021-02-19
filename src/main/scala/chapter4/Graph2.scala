package chapter4

import edu.princeton.cs.algs4.{IndexMinPQ, MinPQ, Queue, UF}
import tools.Utils

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.jdk.CollectionConverters.IterableHasAsScala

//带有权重的边 API
case class Edge(private val v:Int,
                private val w:Int,
                weight: Double) extends Comparable[Edge] { //边的权重
  //边的两个端点之一
  def either(): Int = v
  //边的另一个顶点
  def other(vertex:Int): Int =
    if (vertex == v) w else if (vertex == w) v else
    throw new RuntimeException("Inconsistent edge")
  //字符串表示
  override def toString: String = s"$v-$w $weight"
  //和另一个边的比较
  override def compareTo(o: Edge): Int =
    if (weight < o.weight) -1 else if (weight > o.weight) 1 else 0
}

//带有权重的无向图 API
class EdgeWeightedGraph(var V:Int,//所有顶点个数
                        var E:Int) { //所有边的个数
  def this(file:File) = {
    this(0,0)
    val source = Source.fromFile(file, "UTF-8")
    val data = source.getLines()
    V = data.next().toInt; E = data.next().toInt
    while (data.hasNext) {
      val line = data.next().split(" ")
      val (v,w,we) = (line(0).toInt,line(1).toInt,line(2).toDouble)
      val edge = Edge(v,w,we)
      addEdge(edge)
    }
    source.close()
  }
  private lazy val bags: Array[ArrayBuffer[Edge]] =
    (0 until V).map(_ => new ArrayBuffer[Edge]()).toArray
  def addEdge(e:Edge): Unit = {
    val v = e.either()
    val w = e.other(v)
    bags(v).addOne(e)
    bags(w).addOne(e)
    E += 1
  } //添加边
  def adj(v:Int): Iterable[Edge] = bags(v)//和 v 相关联的所有边
  def edges(): Iterable[Edge] = {
    val b = new ArrayBuffer[Edge]
    (0 until V).foreach { v =>
      adj(v).foreach { e =>
        if (e.other(v) > v) b.addOne(e) //这里忽略了自环
      }
    }; b
  }//图的所有边
  override def toString: String = s"EdgeWeightedGraph - Node: $V," +
    s"Edge: $E" //字符串表示
}

//最小生成树的 API
trait MST {
  val G: EdgeWeightedGraph
  def edges: Iterable[Edge] //最小生成树的所有边
  def weight: Double //最小生成树的权重
}

//最小生成树的测试用例
object MST extends App {
  def test(fun:EdgeWeightedGraph => MST,
           fileName:String = "data/tinyEWG.txt"): Unit = {
    val file = new File(fileName)
    val G = new EdgeWeightedGraph(file)
    val mst = fun(G)
    mst.edges.foreach { e => println(e) }
    println(mst.weight)
  }
  test(new LazyPrimMST(_))
  test(new PrimMST(_))
  test(new KruskalMST(_))
}

//延时实现（直接易懂）的 Prim 算法
class LazyPrimMST(override val G:EdgeWeightedGraph) extends MST {
  private val marked = new Array[Boolean](G.V)
  private val mst = new Queue[Edge]()
  private val pq = new MinPQ[Edge]()
  init()
  private def visit(v:Int): Unit = {
    marked(v) = true
    G.adj(v).foreach { e =>
      if (!marked(e.other(v))) pq.insert(e)
    }
  }
  private def init(): Unit = {
    visit(0)
    while (!pq.isEmpty) {
      val e = pq.delMin()
      val v = e.either()
      val w = e.other(v)
      if (marked(v) && marked(w)) { } else {
        mst.enqueue(e)
        if (!marked(v)) visit(v)
        if (!marked(w)) visit(w)
      }
    }
  }
  override def edges: Iterable[Edge] = mst.asScala
  override def weight: Double = {
    var sum = 0.0; mst.forEach(e => sum += e.weight); sum
  }
}

//即时实现（空间友好）的 Prim 算法
class PrimMST(override val G:EdgeWeightedGraph) extends MST {
  private val edgeTo = new Array[Edge](G.V) //距离树最近的边
  private val distTo = new Array[Double](G.V) //此边的权重
  private val marked = new Array[Boolean](G.V) //是否在树中
  private val pq = new IndexMinPQ[java.lang.Double](G.V) //有效的横切边
  init()
  private def visit(v:Int): Unit = {
    marked(v) = true
    G.adj(v).foreach { e =>
      val w = e.other(v)
      if (marked(w)) { } else {
        if (e.weight < distTo(w)) {
          edgeTo(w) = e
          distTo(w) = e.weight
          if (pq.contains(w)) pq.changeKey(w,distTo(w))
          else pq.insert(w,distTo(w))
        }
      }
    }
  }
  private def init(): Unit = {
    (0 until G.V).foreach { v => distTo(v) = Double.PositiveInfinity }
    distTo(0) = 0.0
    pq.insert(0,0.0)
    while (!pq.isEmpty) visit(pq.delMin())
  }
  override def edges: Iterable[Edge] = marked.zipWithIndex.collect { case (true,i) => edgeTo(i) }
  override def weight: Double = {
    var sum = 0.0
    marked.zipWithIndex.foreach { case (true, index) =>
      sum += distTo(index)
    }; sum
  }
}

//Kruskal 算法的 MST 实现
class KruskalMST(override val G:EdgeWeightedGraph) extends MST {
  private val mst = new Queue[Edge]()
  init()
  def init(): Unit = {
    val pq = new MinPQ[Edge](G.edges().toArray)
    val uf = new UF(G.V)
    while (!pq.isEmpty && mst.size() < G.V - 1) {
      val e = pq.delMin() //获取最小权重的边和其顶点
      val v = e.either(); val w = e.other(v)
      if (uf.connected(v,w)) { } else { //如果边未失效
        uf.union(v,w) //将分量连接起来
        mst.enqueue(e) //插入边到最小生成树
      }
    }
  }
  override def edges: Iterable[Edge] = mst.asScala
  override def weight: Double = mst.asScala.foldLeft(0.0)((i,e) => i + e.weight)
}