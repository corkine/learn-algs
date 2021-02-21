package chapter5

import edu.princeton.cs.algs4.{BinaryIn, BinaryStdIn, BinaryStdOut, MinPQ}

object HuffmanCompress:
	private var R: Int = 256 //ASCII
	//内部单词查找树树结构
	private case class Node(ch:Char, freq:Int, left:Node, right:Node) 
		extends Comparable[Node]:
		override def compareTo(o: Node): Int = this.freq - o.freq
		def isLeaf: Boolean = left == null && right == null
	//单词查找树构造
	private def buildTrie(freq:Array[Int]): Node =
		val pq = new MinPQ[Node]()
		(0 until R).foreach { c => 
			if freq(c) > 0 then pq.insert(Node(c.toChar,freq(c),null,null))
		}
		while pq.size > 1 do
			val x = pq.delMin()
			val y = pq.delMin()
			val parent = Node('\u0000',x.freq + y.freq, x, y)
			pq.insert(parent)
		end while
		pq.delMin()
	//根据单词查找树构造编译表（入口）
	private def buildCode(root:Node): Array[String] =
		val st = new Array[String](R)
		buildCode(st, root, ""); st
	//根据单词查找树构造编译表
	private def buildCode(st:Array[String],x:Node,s:String): Unit =
		if x.isLeaf then { st(x.ch) = s; return }
		buildCode(st, x.left, s + '0')
		buildCode(st, x.right, s + '1')
	//单词查找树写入比特流
	private def writeTrie(x:Node): Unit =
		if x.isLeaf then
			BinaryStdOut.write(true)
			BinaryStdOut.write(x.ch)
			return
		end if
		BinaryStdOut.write(false)
		writeTrie(x.left)
		writeTrie(x.right)
	//从比特流中解码单词查找树
	private def readTrie: Node =
		if BinaryStdIn.readBoolean() then Node(BinaryStdIn.readChar(),0,null,null)
		else Node('\u0000',0,readTrie,readTrie)
	//主要方法：压缩的实现
	private def compress: Unit =
		val s = BinaryStdIn.readString() //读取输入
		val input = s.toCharArray
		val freq = Array[Int](R) //统计频率
		(0 until input.length).foreach { i => freq(input(i)) += 1}
		val root = buildTrie(freq) //构造霍夫曼编码树
		val st = new Array[String](R) //构造编译表
		buildCode(st, root, "")
		writeTrie(root) //写入单词查找树
		BinaryStdOut.write(input.length) //写入字符总数
		(0 until input.length).foreach { i => 
			val code = st(input(i)) //使用霍夫曼编码树来讲输入文本流转换为字节流
			(0 until code.length).foreach { j =>
				if code.charAt(j) == '1' then BinaryStdOut.write(true)
				else BinaryStdOut.write(false)
			}
		}
		BinaryStdOut.close()
	//主要方法：解压缩的实现
	private def expand: Unit =
		val root = readTrie
		val N = BinaryStdIn.readInt()
		(0 until N).foreach { i =>
			var x = root
			while !x.isLeaf do
				if BinaryStdIn.readBoolean() then x = x.right
				else x = x.left
			end while
			BinaryStdOut.write(x.ch)
		}
		BinaryStdOut.close()
end HuffmanCompress

object LZW {
	private val R = 256
	private val L = 4096
	private val W = 12

	def compress(): Unit = 
		import edu.princeton.cs.algs4.TST
		var input = "ABRACADABRABRABRA"
		val st = new TST[Int]
		for (i <- 0 until R) {
			st.put("" + i.toChar, i)
		}
		var code = R + 1 // R为文件结束(EOF)的编码
		while input.length > 0 do
			val s = st.longestPrefixOf(input) // 找到匹配的最长前缀 
			BinaryStdOut.write(st.get(s), W); // 打印出s的编码
			val t = s.length
			if t < input.length && code < L then // 将s加入符号表
				val res = input.substring(0, t + 1)
				st.put(res, {code += 1; code - 1 })
			end if
			input = input.substring(t) // 从输入中读取s
		end while
		BinaryStdOut.write(R, W) // 写入文件结束标记
		BinaryStdOut.close()
	end compress
	
	def expand(): Unit =
		val st = new Array[String](L)
		var i: Int = 0 //下一个待补全的编码值
		(0 until R).foreach { i => st(i) = "" + 
			i.asInstanceOf[Char] } //使用字符初始化编译表
		st({i += 1; i}) = " " //(未使用) EOF 的前瞻字符
		var codeword = BinaryStdIn.readInt(W)
		var v = st(codeword)
		var break = false
		while !break do
			BinaryStdOut.write(v) //输出当前子字符串
			codeword = BinaryStdIn.readInt(W)
			if codeword == R then break = true //文件结束
			else 
				var s = st(codeword) //获取下一个编码值
				//前瞻字符不可用 83 == 83 见特殊情况
				if i == codeword then s = v + v.charAt(0) 
				//根据上一个字符串首字母得到编码的字符串
				if i < L then st({i += 1; i}) = v + s.charAt(0) 
				v = s //更新当前编码
		BinaryStdOut.close()
	end expand
	
	def main(args: Array[String]): Unit = {
		LZW.compress()
	}
}