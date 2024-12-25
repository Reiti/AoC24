import util.Day

object Day25 extends Day(25):
  override def solve(): Unit =
    val (l, k) = input.split("\n\n").partition(s => s.split("\n").head == "#####")
    val locks = l.map(e => e.split("\n").map(_.toCharArray).transpose.map(p => p.count(_ == '#') - 1))
    val keys = k.map(e => e.split("\n").map(_.toCharArray).transpose.map(p => p.count(_ == '#') - 1))

    //Part 1
    println(keys.map(k => locks.count(l => l.zip(k).map(_ + _).forall(_ <= 5))).sum)