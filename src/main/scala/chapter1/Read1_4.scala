package chapter1.Read1_4

import edu.princeton.cs.algs4.BinarySearch
import tools.Utils

import scala.io.Source
import scala.util.Sorting

object ThreeSumDemo extends App {
  val source = Source.fromFile("data/1Kints.txt")
  val data = source.getLines().toArray.map(_.trim.toInt)
  source.close()
  var count = 0
/*  for {
    a <- data.indices
    b <- (a + 1) until data.length
    c <- (b + 1) until data.length
  } yield {
    if (data(a) + data(b) + data(c) == 0) count += 1
  }*/ //4s
  Utils.ptime1 {
    data.indices.foreach { a =>
      (a until data.length).foreach { b =>
        (b until data.length).foreach { c =>
          if (a != b && b != c) {
            if (data(a) + data(b) + data(c) == 0) {
              //println(a,b,c,data(a),data(b),data(c))
              count += 1
            }
          }
        }
      }
    } //0.345s
  }
  println(count)
}

object TwoSumFastDemo extends App {
  val source = Source.fromFile("data/8Kints.txt")
  val data = source.getLines().toArray.map(_.trim.toInt)
  source.close()
  Sorting.quickSort(data)
  var count = 0
  Utils.ptime1 {
    data.indices.foreach { i =>
      if (BinarySearch.indexOf(data, -data(i)) > 0) count += 1
    } //0.345s
  }
  println(count)
}

object ThreeSumFastDemo extends App {
  val source = Source.fromFile("data/8Kints.txt")
  val data = source.getLines().toArray.map(_.trim.toInt)
  source.close()
  Sorting.quickSort(data)
  var count = 0
  Utils.ptime3 {
    data.indices.foreach { i =>
      ((i + 1) until data.length).foreach { j =>
        if (BinarySearch.indexOf(data, -data(i) - data(j)) > j) count += 1
      }
    } //0.345s
  }
  println(count)
}