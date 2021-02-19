package week3

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.control.Button
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.{Group, Scene}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object LineDraw extends JFXApp {
  val points = ArrayBuffer(
    Circle(100,200,5),
    Circle(150,300,5),
    Circle(140,280,5),
    Circle(170,340,5),
    Circle(100,300,5),
    Circle(100,400,5),
    Circle(100,500,5),
    Circle(120,300,5),
    Circle(300,400,5),
    Circle(150,200,5),
    Circle(100,330,5),
  )
  val points2 = Array.fill(10) {
    Circle(Random.nextInt(400) + 30, Random.nextInt(400) + 30, 5)
  }

  val pointsJava: Array[Point] = points.map(c => new Point(c.centerX().toInt,c.centerY().toInt)).toArray
  val bcPoints = new FastCollinearPoints(pointsJava)
  val add: Button = new Button("Show") {
    translateX = 15
    translateY = 15
    onAction = _ => {
      bcPoints.segments().foreach { ls =>
        println("seg is " -> ls)
        val l = Line(ls.p.x,ls.p.y,ls.q.x,ls.q.y)
        l.stroke = Color.Red
        l.strokeWidth = 3
        group.children.add(l)
      }
    }
  }
  val group: Group = new Group {
    children = Seq(add) ++ points
  }
  stage = new PrimaryStage {
    scene = new Scene(500,500) {
      root = group
    }
  }

}
