import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.util.Using
import scala.collection.mutable.Map

sealed trait Event
case object WakeUp extends Event
case object Asleep extends Event
case class Starts(id: Int) extends Event



object Day04 extends App {
  def mkMinutes = Array.fill(59)(0)

  val parser = """ (\d{2}):(\d{2})\] (?:(falls)|(wakes)|Guard #(\d+))""".r
  def parseEvent(s: String) : (Int, Event) = {
    parser.findFirstMatchIn(s) match {
      case Some(m) => {
        var minute = m.group(2).toInt
        if (m.group(1) == "23") {
          minute = 0
        }
        val ev =
          if (m.group(3) != null)
            Asleep
          else if (m.group(4) != null) {
            WakeUp
          } else
            Starts(m.group(5).toInt)
        (minute, ev)
      }
      case None => throw new RuntimeException(s)
    }
  }

  val events = Using(scala.io.Source.fromFile("./data/04-1.txt")) { file =>
    file.getLines().toList.sorted.map(parseEvent(_))
  }

  var currentGuard = 0
  var prevSec = 0
  var isAsleep = false
  var guardPresence : Map[Int, Array[Int]] = Map()

  def addSpan(lastSec: Int): Unit = {
    if (!guardPresence.contains(currentGuard)) {
      guardPresence(currentGuard) = mkMinutes
    }
    val perMin = guardPresence(currentGuard)
    for (sec <- prevSec to (lastSec-1)) {
      perMin(sec) += 1
    }
  }

  for ((sec, evt) <- events.get) {
    evt match {
      case Starts(guard) =>
        if (isAsleep) {
          addSpan(60)
        }
        currentGuard = guard
        isAsleep = false
      case Asleep =>
        isAsleep = true
      case WakeUp =>
        isAsleep = false
        addSpan(sec)
    }
    prevSec = sec
  }
  if (isAsleep) {
    addSpan(60)
  }

  var maxAsleep = 0
  var maxId = 0
  guardPresence.foreachEntry{ (g, pres) =>
    val asleep = pres.sum
    if (asleep > maxAsleep) {
      maxAsleep = asleep
      maxId = g
    }
  }

  println(maxId)
  println(maxAsleep)
  val pickedMin = guardPresence(maxId).zipWithIndex.maxBy(_._1)._2
  println(pickedMin)
  println(maxId * pickedMin)

  val bestMinute = guardPresence.map { case (gu, arr) =>
    (gu, arr.zipWithIndex.maxBy(_._1))
  }
  val (g, (ct, min)) = bestMinute.maxBy(_._2._1)
  println(g * min)

}
