import util.{Day, Util}

object Day13 extends Day(13):
  override def solve(): Unit =
    val machines = input.split("\n\n").map(_.split("\n").map(Util.longs).map{ case Seq(x, y) => (x, y) })

    //Part 1
    println(machines.flatMap(tokenCost).sum)

    //Part 2
    println(machines.map{case Array(a, b, price) => Array(a, b, (price._1 + 10000000000000L, price._2 + 10000000000000L))}.flatMap(tokenCost).sum)
  
  def countPresses(a: (Long, Long), b: (Long, Long), price: (Long, Long)): Option[(Long, Long)] =
    val bp = (a._1 * price._2 - a._2 * price._1).toDouble / (a._1 * b._2 - a._2 * b._1).toDouble
    val ap = (price._1 - b._1 * bp) / a._1

    if ap.isWhole && bp.isWhole then
      Some((ap.toLong, bp.toLong))
    else
      None

  def tokenCost(machine: Array[(Long, Long)]): Option[Long] = machine match
    case Array(a, b, price) => countPresses(a, b, price).map((a, b) => 3L*a + b)