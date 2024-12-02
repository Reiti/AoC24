import util.Day

object Day1 extends Day(1):
  override def solve(): Unit =
    val (l, r) = inputLines.map(_.split(" {3}")).map(r => (r(0).toInt, r(1).toInt)).unzip

    //Part 1
    println(l.sorted.zip(r.sorted).map((l, r) => (l - r).abs).sum)

    val counts = r.map(i => i -> r.count(_ == i)).toMap.withDefaultValue(0)

    //Part 2
    println(l.map(i => i * counts(i)).sum)