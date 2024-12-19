import util.{Day, Util}

object Day19 extends Day(19):
  override def solve(): Unit =
    val Array(t, p) = input.split("\n\n")
    val towels = t.split(", ").map(_.trim)
    val patterns = p.split("\n")

    //Part 1
    println(patterns.count(p => countPossibilities(p, towels) > 0))

    //Part 2
    println(patterns.map(p => countPossibilities(p, towels)).sum)

  lazy val countPossibilities: ((String, Array[String])) => Long = Util.memoize:
    case ("", _) =>
      1L
    case (pattern, towels) =>
      towels.filter(pattern.startsWith).map(p => countPossibilities(pattern.drop(p.length), towels)).sum