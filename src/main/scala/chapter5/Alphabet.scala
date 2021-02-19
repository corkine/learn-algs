package chapter5

import edu.princeton.cs.algs4.{StdIn, StdOut}

trait Alphabet {
  def toChar(index:Int):Char
  def toIndex(c:Char):Int
  def contains(c:Char):Boolean
  def R:Int //Alphabet 字符数量
  def lgR:Int //Alphabet 表示一个索引需要的位数
  def toIndices(s:String):Array[Int] //字符串转换为 R 进制整数
  def toChars(indices:Array[Int]):String //R 进制整数转换为字符串
}

object Alphabet {
  val BINARY: Alphabet = new AlphabetImpl("01")
  val OCTAL: Alphabet = new AlphabetImpl("01234567")
  val DECIMAL: Alphabet = new AlphabetImpl("0123456789")
  val HEXADECIMAL: Alphabet = new AlphabetImpl("0123456789ABCDEF")
  val DNA: Alphabet = new AlphabetImpl("ACGT")
  val LOWERCASE: Alphabet = new AlphabetImpl("abcdefghijklmnopqrstuvwxyz")
  val UPPERCASE: Alphabet = new AlphabetImpl("ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  val PROTEIN: Alphabet = new AlphabetImpl("ACDEFGHIKLMNPQRSTVWY")
  val BASE64: Alphabet = new AlphabetImpl("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
  val ASCII: Alphabet = new AlphabetImpl(128)
  val EXTENDED_ASCII: Alphabet = new AlphabetImpl(256)
  val UNICODE16: Alphabet = new AlphabetImpl(65536)
  def of(s:String): Alphabet = new AlphabetImpl(s)
  def of(i:Int = 256): Alphabet = new AlphabetImpl(i)

  def main(args: Array[String]): Unit = {
    val simple = "AACGAACGGTTTACCCCG"
    val ints = Alphabet.DNA.toIndices(simple)
    val str = Alphabet.DNA.toChars(ints)
    println((str, simple, str == simple))
    simpleUsage()
  }

  def simpleUsage(content:String="ABRACADABRA!"): Unit = {
    val element = content.replace("!","").toCharArray.distinct.mkString("")
    val alphabet = Alphabet.of(element)
    val R = alphabet.R
    val count = new Array[Int](R)
    content.toCharArray.foreach { c =>
      if (alphabet.contains(c)) count(alphabet.toIndex(c)) += 1
    }
    (0 until R).foreach(i =>
      println(alphabet.toChar(i) + " " + count(i)))
  }
}

private class AlphabetImpl private() extends Alphabet {
  def this(s:String) = {
    this()
    val unicode = Array.fill(Char.MaxValue)(false)
    (0 until s.length).foreach { i =>
      val c = s.charAt(i)
      if (unicode(c)) throw new IllegalArgumentException(s"Repeat of $c")
      unicode(c) = true
    }
    alphabet = s.toCharArray
    r = s.length
    inverse = Array.fill(Char.MaxValue)(-1)
    (0 until R).foreach { c => inverse(alphabet(c)) = c }
  }
  def this(i:Int) = {
    this()
    r = i
    alphabet = (0 until R).map(_.toChar).toArray
    inverse = (0 until R).toArray
  }
  private var alphabet: Array[Char] = _
  private var r: Int = _
  private var inverse: Array[Int] = _
  override def toChar(index: Int): Char = {
    if (index < 0 || index >= R)
      throw new IllegalArgumentException(s"Index must between 0 and $R")
    alphabet(index)
  }
  override def toIndex(c: Char): Int = {
    if (c >= inverse.length || inverse(c) == -1)
      throw new IllegalArgumentException(s"Character $c not in alphabet")
    inverse(c)
  }
  override def contains(c: Char): Boolean = inverse(c) != -1
  override def R: Int = r
  override def lgR: Int = {
    var lgR = 0
    var t = R - 1
    while (t >= 1) {
      t = t / 2
      lgR += 1
    }; lgR
  }
  override def toIndices(s: String): Array[Int] = {
    val source = s.toCharArray
    val target = Array.fill(s.length)(0)
    (0 until source.length).foreach { i =>
      target(i) = toIndex(source(i))}; target
  }
  override def toChars(indices: Array[Int]): String = {
    val sb = new StringBuilder(indices.length)
    indices.indices.foreach {
      i => sb.append(toChar(indices(i)))}
    sb.toString()
  }
}

trait Basic {
case class Student(name:String,key:Int)
  def keyIndexedCounting(a:Array[Student]): Unit = {
    val N: Int = a.length //总的人数
    val R: Int = a.map(_.key).distinct.length //总的组数
    val aux = new Array[Student](N) //临时数组
    val count = new Array[Int](R + 1) //频率（第一步） & 组标记（第二步）数组
    // 1. 频率统计
    // 注意，组这里有 1，2，3，4 组，其分别位于索引的 2，3，4，5 位置，其余留空
    for (i <- 0 until N) count(a(i).key + 1) += 1
    // 2. 频率转换为索引
    // 遍历频率数组，为每个组确定其位置 —— 通过将之前组的频率累加得到
    // 此时索引 1，2，3，4 位置分别表示了组 1，2，3，4 的索引位置，这也是为什么
    // 在第一步需要 a(i).key + 1 的原因。
    var r = 0
    while (r < R) { count(r + 1) += count(r); r += 1 }
    // 3. 数据分类
    // 对于每个学生，处理后都将其组标记起始位置向后推 1
    // 之后将其插入到原来的位置中
    for (i <- 0 until N) aux({
        count(a(i).key) += 1; count(a(i).key) - 1
    }) = a(i)
    // 4. 将结果写回到原始数组
    for (i <- 0 until N) { a(i) = aux(i) }
  }
}