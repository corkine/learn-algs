package week1

trait Conn {
  val end:Int
  lazy val ids:Array[Int] = (0 to end).toArray
  def connect(a:Int,b:Int):Boolean
  def union(a:Int,b:Int):Unit
}
/**
 * Quick Find (Eager)
 * Init - N, Union - N, Find - 1
 * 实现：对每个数据进行 id 标定，然后对每个数据的连接进行全 id 数组遍历，将其 id 修改一致
 * 缺点：对于 N 个对象进行 N 次 Union，则为 o2 复杂度，每次 union 都需要遍历一次数组
 */
trait QuickFind extends Conn {
  override def connect(a:Int,b:Int):Boolean = ids(a) == ids(b)
  override def union(a:Int,b:Int): Unit = {
    val aId = ids(a)
    val bId = ids(b)
    ids.indices.foreach { i =>
      val id = ids(i)
      if (id == aId) ids(i) = bId
    }
    println("ids",ids.mkString(","))
  }
}

/**
 * Quick Union (Lazy)
 * Init - N, Union - N+, Find - N
 * 实现：对每个分组使用树表示，Union 将树组合起来，Connect 遍历其 Root 查看是否相连
 * 缺点：可能存在较高的树，其 connect 查询将耗费巨大 - 参见 WeightQuickUnion
 */
trait QuickUnion extends Conn {
  override def connect(a: Int, b: Int): Boolean = root(a) == root(b)
  override def union(a: Int, b: Int): Unit = {
    ids(root(a)) = root(b)
    println("ids",ids.mkString(","))
  }
  protected def root(a:Int):Int = ids(a) match {
    case `a` => a
    case parent => root(parent)
  }
}

/**
 * Weight Quick Union
 * Init - N, Union - lgN, Find - lgN
 * 实现：在 QuickUnion 基础上进行树大小对比，始终将小树放在大数下，防止树过高
 */
trait WeightQuickUnion extends Conn {
  protected lazy val sizes: Array[Int] = Array.fill(ids.length)(1)
  protected def root(a:Int):Int = ids(a) match {
    case `a` => a
    case parent => root(parent)
  }
  override def connect(a: Int, b: Int): Boolean = root(a) == root(b)
  override def union(a: Int, b: Int): Unit = {
    val ar = root(a)
    val br = root(b)
    if (ar == br) return
    val sa = sizes(a)
    val sb = sizes(b)
    val sab = sa + sb
    if (sa <= sb) ids(ar) = br else ids(br) = ar
    sizes(b) = sab
    sizes(a) = sab
    print("ids",ids.mkString(","))
    println("sizes",sizes.mkString(","))
  }
}

/**
 * PathZipped Weight Quick Union
 * Init - N, Union/Find - N + MlgN
 * 实现：在遍历 root 时将树展平，进一步提升树的广度/降低高度
 */
trait PathZipped { self: WeightQuickUnion =>
  override protected def root(a: Int): Int = ids(a) match {
    case `a` => a
    case parent =>
      ids(a) = ids(ids(a))
      root(parent)
  }
}

object QuickFindTest extends App {
  val demo = new WeightQuickUnion {
    val end = 10
  }
  demo.union(9,0)
  demo.union(3,4)
  demo.union(5,8)
  demo.union(7,2)
  demo.union(2,1)
  demo.union(5,7)
  demo.union(0,3)
  demo.union(4,2)
  (0 to demo.end).flatMap(a => (0 to demo.end).map(b => (a,b))).foreach { case (a,b) =>
    println(s"$a ${if (demo.connect(a,b)) "Connect" else "DisConnect"} $b")
  }
}