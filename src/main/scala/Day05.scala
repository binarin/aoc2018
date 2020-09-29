import scala.io.Source

object Day05 extends App {
  val it = Source.fromFile("./data/05-1.txt").getLines().toList(0)
  //  val it = "dabAcCaCBAcCcaDA".toList
  def react(inp: String): String = {
    var input = inp.toList
    var output: List[Char] = List()

    while (input.length > 0) {
      val hasTwo = output.take(2).length == 2
      if (hasTwo && output(0) != output(1) && output(0).toLower == output(1).toLower) {
        output = output.drop(2)
      } else {
        output = input.head :: output
        input = input.tail
      }
    }
    if (output(0) != output(1) && output(0).toLower == output(1).toLower) {
      output = output.drop(2)
    }
    output.reverse.mkString("")
  }

  for (ch <- 'a' to 'z') {
    val subseq = it.filter(_.toLower != ch)
    print(ch)
    print(" ")
    println(react(subseq).length)
  }

  println(react("dabAcCaCBAcCcaDA"))

}