package chapter1.Ex1_2

import java.awt.geom.Point2D
import java.time.{DayOfWeek, LocalDate}

import chapter1.Ex1_2.N11.SmartDate.{Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday, Week}
import edu.princeton.cs.algs4.{Date, StdDraw}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.StringBinding
import scalafx.beans.property.{IntegerProperty, LongProperty}
import scalafx.geometry.Pos
import scalafx.scene.control.{Button, Label, ProgressBar}
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Font
import scalafx.scene.{Group, Scene}

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn
import scala.util.Random
import scala.util.matching.Regex

object N1 extends App {
  def minDistance(points:Array[Point2D]): Double = {
    val res = ArrayBuffer[(Point2D,Point2D)]()
    points.foreach { a =>
      points.foreach { b =>
        if (a != b) res.append(a -> b)
      }
    }
    res.map { case (a, b) =>
      Math.sqrt(Math.pow(a.getX - b.getX,2) + Math.pow(a.getY - b.getY,2)) }.min
  }
  println("Input Number >> ")
  println("MinDistance is ", minDistance {
    (1 to StdIn.readInt()).foldLeft (Array[Point2D]()) { (sum, _) =>
      sum :+ new Point2D.Double(Random.nextDouble(),Random.nextDouble())
    }
  })
}

object N2 extends App {
  case class Interval1D(min:Double,max:Double) {
    assert(min <= max)
  }
  def printCrossInterval1D(in:Array[Interval1D]):String = {
    val res = ArrayBuffer[(Interval1D,Interval1D)]()
    in.foreach(a => in.foreach(b => if (a != b) res.append(a -> b)))
    res.filter{ case (x,y) =>
      if (x.max <= y.min || x.min >= y.max) false
      else true
    }.map{ case (x,y) =>
      if (x.hashCode() > y.hashCode()) x -> y else y -> x }.toSet.mkString("\n")
  }
  println("Enter a number, and enter all intervals line by line>>")
  val n = StdIn.readInt()
  var now = 1
  val data = ArrayBuffer[Interval1D]()
  while (now <= n) {
    val ans = StdIn.readLine("A Interval >> ").split(" ")
    data.append(Interval1D(ans(0).toDouble,ans(1).toDouble))
    now += 1
  }
  println(printCrossInterval1D(data.toArray))
}

trait N3 {
  case class Interval2D(x:Double,y:Double)
  private val conf = StdIn.readLine("Input Number, minValue, maxValue by space >>")
                  .split(" ").map(_.trim)
  //eg. 5 0 500
  val (number, min, max) = (conf(0).toInt, conf(1).toInt, conf(2).toInt)
  def randomLength: Double = Random.nextDouble() * (max - min) + min
  val points: ArrayBuffer[Interval2D] = (1 to number).foldLeft(ArrayBuffer[Interval2D]()) { (s, _) =>
    s :+ Interval2D(randomLength, randomLength)
  }
  def isCross(a:Interval2D,b:Interval2D):Boolean = !{
    (Math.abs(b.x - a.x) >= 100) || (Math.abs(b.y - a.y) >= 100)
  }
  (for {
    a <- points
    b <- points if a != b
  } yield {
    if (a.hashCode() > b.hashCode()) {
      (a,b,isCross(a,b))
    } else {
      (b,a,isCross(a,b))
    }
  }).toSet.foreach(println)
  /*StdDraw.setPenColor(StdDraw.RED)
  points.foreach{ i =>
    println(i)
    StdDraw.filledSquare(i.x + 50,i.y + 50,50)
  }*/
}

object SFXShow extends JFXApp with N3 {
  val pointsFX: ArrayBuffer[Rectangle] = points.map { i =>
    val r = Rectangle(100,100,Color.Red)
    r.setOpacity(0.4)
    r.setX(i.x)
    r.setY(i.y)
    r
  }
  stage = new PrimaryStage {
    scene = new Scene(600,600) {
      root = new Group {
        children = pointsFX
      }
    }
  }
}

object N4 extends App {
  /*val a = "hello"
  val b = a
  a = "world"
  println(a)
  println(b)*/
}

object N5 extends App {
  val s = "Hello World"
  s.toUpperCase()
  s.substring(6,11)
  println(s)
}

object N6 extends App {
  def isRotation(a:String, b:String):Boolean = {
    a.length == b.length && a.concat(a).indexOf(b) >= 0
  }
  private val a: String = StdIn.readLine("Input str A:")
  private val b: String = StdIn.readLine("Input str B:")
  println("IS a rotation? ", isRotation(a,b))
}

object N7 extends App {
  def mystery(s:String): String = {
    val N = s.length
    if (N <= 1) return s
    val a = s.substring(0, N/2)
    val b = s.substring(N/2,N)
    mystery(a) + mystery(b)
  }
  println(mystery("HelloWorld"))
}

object N9 extends App {
  class Counter {
    private var nowNumber:Long = 0
    def increment():Unit = nowNumber += 1
    def clear():Unit = nowNumber = 0
    def now:Long = nowNumber
  }
  def rank(a:Array[Int],b:Int,counter: Counter):Int = {
    counter.clear()
    def loop(a:Array[Int],b:Int,lo:Int,hi:Int):Int = {
//      Thread.sleep(1000)
      if (lo > hi) -1
      else {
        val mid = (lo + hi)/2
        counter.increment()
        val midValue = a(mid)
        //注意，这里如果是 mid，而非 mid+1 则可能造成死循环，比如卡在 6，7
        if (b > midValue) loop(a,b,mid+1,hi)
        else if (b < midValue) loop(a,b,lo,mid-1)
        else mid
      }
    }
    loop(a,b,0,a.length-1)
  }
  val counter = new Counter
  println(rank(Array(1,2,3,4,9,12,111,1992),111,counter))
  println(counter.now)
}

trait N10 {
  class Counter(private var number:Long = 0, val max:Long = 100) {
    def increment():Unit = {
      if (number >= max) {}
      else number += 1
    }
    def decrement():Unit = number -= 1
    def clear():Unit = number = 0
    def now:Long = number
  }
  val counter = new Counter(max = 20)
}

object N10App extends JFXApp with N10 {
  val state = LongProperty(0)
  stage = new PrimaryStage { scene = new Scene(400,300) {
    root = new VBox {
      spacing = 15
      alignment = Pos.Center
      children = Seq(
        new Label { text <== state.asString(); font = Font.font(45) },
        new ProgressBar {
          progress <== state * 1.0 / counter.max
        },
        new HBox {
          alignment = Pos.Center
          spacing = 15
          children = Seq(
            new Button("Clear") {
              onAction = _ => {counter.clear(); state.set(counter.now)}
            },
            new Button("+1") {
              onAction = _ => {counter.increment()
                state.set(counter.now)
              }
            },
            new Button("-1") {
              onAction = _ => {counter.decrement()
                state.set(counter.now)}
            }
          )
        }
      )
    }
  }}
}

object N11 extends App {
  class SmartDate(date:String) extends Date(date) {
    if (!date.contains("/")) throw new RuntimeException("解析错误")
    def dayOfTheWeek():Week = LocalDate.of(this.year(),this.month(),this.day()).getDayOfWeek match {
      case DayOfWeek.MONDAY => Monday
      case DayOfWeek.TUESDAY => Tuesday
      case DayOfWeek.WEDNESDAY => Wednesday
      case DayOfWeek.THURSDAY => Thursday
      case DayOfWeek.FRIDAY => Friday
      case DayOfWeek.SATURDAY => Saturday
      case DayOfWeek.SUNDAY => Sunday
    }

    override def equals(other: Any): Boolean = other match {
      case i: SmartDate =>i.year() == this.year() && i.month() == this.month() && i.day() == this.day()
      case _ => super.equals(other)
    }
  }
  object SmartDate {
    trait Week
    object Monday extends Week
    object Tuesday extends Week
    object Wednesday extends Week
    object Friday extends Week
    object Saturday extends Week
    object Thursday extends Week
    object Sunday extends Week
  }
  val a = new SmartDate("10/11/2020")
  println(a.month())
  println(a.dayOfTheWeek())
}

