import scala.io.Source
import scala.util.parsing.combinator._

case class Claim (id: Int, left: Int, top: Int, width: Int, height: Int)

object ClaimParser extends RegexParsers {
  def line: Parser[Claim] = ("#" ~> num) ~ ("@" ~> num) ~ ("," ~> num) ~ (": " ~> num) ~ ("x" ~> num) ^^ {
    case id ~ left ~ top ~ width ~ height => Claim(id, left, top, width, height)
  }
  def num: Parser[Int] = "\\d+".r ^^ { _.toInt }
}

object Day03 extends App {
  val input = Source.fromFile("./data/03-1.txt")
  val lines = try input.getLines().toSeq finally input.close()
  val parsed = lines map (ClaimParser.parse(ClaimParser.line, _).get)

  val fabricWidth = 1000
  val fabricHeight = 1000
  val fabric = Array.ofDim[Int](fabricWidth * fabricHeight)

  def sample = Seq(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2",
  ).map(ClaimParser.parse(ClaimParser.line, _).get)

  def expandClaim(c: Claim): Seq[Int] = c match {
    case Claim(_, left, top, width, height) =>
      for {
        y <- top to (top + height - 1)
        x <- left to (left + width - 1)
      } yield (y*fabricWidth + x)
  }

  for (c <- parsed) {
    for (coord <- expandClaim(c)) {
      fabric(coord) += 1
    }
  }

  println(fabric.count(_ > 1))

  for (c <- parsed) {
    if (expandClaim(c).forall(fabric(_) == 1)) {
      println(c)
    }
  }
}
