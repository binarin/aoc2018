import scala.collection.mutable.{Map, Set}
import scala.io.Source
object Day02 extends App {
  def letterCount(s: String): Map[Char, Int] = {
    val m = Map[Char, Int]()
    s.foreach {c =>
      m(c) = m.getOrElse(c, 0) + 1
    }
    m
  }

  def classify(s: String) : (Int, Int) = {
    (if (letterCount(s).values.exists(_ == 2)) 1 else 0,
      if (letterCount(s).values.exists(_ == 3)) 1 else 0
    )
  }

  def checksum(ss: Iterable[String]) : Int = {
    val (a, b) = ss.map(classify).fold((0, 0)) { case ((a1, b1), (a2, b2)) => (a1+a2, b1+b2) }
    a * b
  }

  def sample = Iterable(
    "abcdef",
    "bababc",
    "abbcde",
    "abcccd",
    "aabcdd",
    "abcdee",
    "ababab"
  )

  val lines = Source.fromFile("./data/02-1.txt").getLines().to(Seq)
  println(checksum(sample))
  println(checksum(lines))

  val width = lines(0).length

  for (pos <- 0 until (width - 1)) {
    val seen = Set[String]()
    for (cur <- lines) {
      val common = cur.substring(0, pos) + cur.substring(pos + 1)
      if (seen(common)) {
        println(common)
      } else {
        seen += common
      }
    }
  }



}
