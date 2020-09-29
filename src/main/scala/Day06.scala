import scala.io.Source
import scala.collection.mutable.Map
import scala.math.abs

sealed trait Cell
case object Unknown extends Cell
case class Conflict(dist: Int) extends Cell
case class CloseTo(id: Int, dist: Int) extends Cell

class Grid(minX: Int, maxX: Int, minY: Int, maxY: Int) {
  val width = maxX - minX + 1
  val height = maxY - minY + 1
  val arr : Array[Cell] = Array.fill(width * height)(Unknown)

  def apply(x: Int, y: Int): Cell = {
    arr(x-minX+width*(y-minY))
  }

  def update(x: Int, y: Int, c: Cell) = {
    // println(s"($x, $y) = $c")
    arr(x-minX+width*(y-minY)) = c
  }

  def addDist(x: Int, y: Int, fromX: Int, fromY: Int, id: Int): Unit = {
    val dist = abs(x - fromX) + abs(y - fromY)
    this(x, y) match {
      case Unknown => this(x,y) = CloseTo(id, dist)
      case Conflict(oldDist) if oldDist > dist => this(x,y) = CloseTo(id, dist)
      case CloseTo(oldId, oldDist) if oldDist > dist => this(x, y) = CloseTo(id, dist)
      case CloseTo(oldId, oldDist) if oldDist == dist => this(x, y) = Conflict(dist)
      case _ => ()
    }
  }
  def sumDist(x: Int, y: Int, fromX: Int, fromY: Int): Unit = {
    val dist = abs(x - fromX) + abs(y - fromY)
    this(x, y) match {
      case Unknown => this(x, y) = CloseTo(0, dist)
      case CloseTo(_, oldDist) => this(x, y) = CloseTo(0, dist + oldDist)
    }
  }

  def renderCell(c: Cell) : String = c match {
    case Unknown => "!"
    case Conflict(_) => "."
    case CloseTo(idx, _) => idx.toString
  }

  def render() = {
    for ( y <- 0 until height ) {
      for ( x <- 0 until width) {
        print(renderCell(arr(x + y * width)))
      }
      println()
    }
  }
}

object Day06 extends App {
  val src = Source.fromFile("./data/06-1.txt")
  val coords = src.getLines().map(_.split(", ").map(_.toInt)).toList.map(arr => (arr(0), arr(1)))
  val xs = coords map {
    _._1
  }
  val ys = coords map {
    _._2
  }
  val minX = xs.min - 1
  val maxX = xs.max + 1
  val minY = ys.min - 1
  val maxY = ys.max + 1
  val infs = Array.fill(coords.length)(false)
  val counts = Array.fill(coords.length)(0)

  val gr = new Grid(minX, maxX, minY, maxY)

  for {
    (cc@(fx, fy), idx) <- coords.zipWithIndex
    x <- minX to maxX
    y <- minY to maxY
  } {
    gr.addDist(x, y, fx, fy, idx)
  }

  def addInf(c: Cell) = c match {
    case CloseTo(idx, _) => infs(idx) = true
    case _ => ()
  }

  for (x <- minX to maxX) {
    addInf(gr(x, minY))
    addInf(gr(x, maxY))
  }

  for (y <- minY to maxY) {
    addInf(gr(minX, y))
    addInf(gr(maxX, y))
  }


  gr.arr foreach {
    case CloseTo(idx, _) => counts(idx) += 1
    case _ => ()
  }

  println(infs.zip(counts).filter(!_._1).map(_._2).max)

  val gr2 = new Grid(minX, maxX, minY, maxY)

  for {
    ((fx, fy), idx) <- coords.zipWithIndex
    x <- minX to maxX
    y <- minY to maxY
  } {
    gr2.sumDist(x, y, fx, fy)
  }

  val ans = gr2.arr map { case CloseTo(id, dist) => dist } filter {
    _ < 10000
  }

  println(ans.length)
}