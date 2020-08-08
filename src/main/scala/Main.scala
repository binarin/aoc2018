import scala.io.Source
import scala.collection.mutable.Set


object Day01 extends App {
  val deltas = Source.fromFile("data/01-1.txt").getLines().map (_.toInt).to(List)

  println(deltas.sum)

  var repeated = LazyList.continually(deltas.to(LazyList)).flatten

  val seen = Set[Int]()

  var freq = 0

  var done = false

  var count = 0

  while (!done) {
    count += 1
    val cur = repeated.head
    repeated = repeated.tail

    freq += cur
    if (seen(freq)) {
      println(freq)
      done = true;
    }

    seen.add(freq)
  }

  println(count)

}
