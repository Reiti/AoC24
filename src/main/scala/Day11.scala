import util.{Day, Util}

object Day11 extends Day(11):
  override def solve(): Unit =
    val stones = input.split(" ")

    //Part 1
    println(stones.map(s => countStones(s, 25)).sum)

    //Part 2
    println(stones.map(s => countStones(s, 75)).sum)

  lazy val countStones: ((String, Int)) => Long = Util.memoize {
    case (_, 0) => 1
    case ("0", blinks) => countStones("1", blinks - 1)
    case (num, blinks) if num.length % 2 == 0 => num.grouped(num.length / 2).map(s => countStones(s.toLong.toString, blinks - 1)).sum
    case (num, blinks) => countStones((num.toLong * 2024L).toString, blinks - 1)
  }