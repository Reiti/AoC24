import util.Day
import util.Util._

object Day8 extends Day(8):
  override def solve(): Unit =
    val antennas = inputMap.filter(e => e._2 != '.')
    val frequencies = antennas.values.toSet

    val antiNodes = frequencies.flatMap(f => antennas.filter(_._2 == f).keys.toList.combinations(2).flatMap(l => findAntiNodes(l.head, l(1))))

    //Part 1
    println(antiNodes.count(inputMap.contains))

    val antiNodesWithHarmonics = frequencies.flatMap(f => antennas.filter(_._2 == f).keys.toList.combinations(2).flatMap(l => findAntiNodesWithHarmonics(l.head, l(1), inputMap)))

    //Part 2
    println(antiNodesWithHarmonics.size)

  def findAntiNodes(a1: Pos, a2: Pos): List[Pos] =
    val slope = a1 - a2
    List(a1 + slope, a2 - slope)

  def findAntiNodesWithHarmonics(a1: Pos, a2: Pos, inputMap: Map[Pos, Char]): Set[Pos] =
    val slope = a1 - a2

    val anti1 = LazyList.from(1).map(s => a1 + slope*s).takeWhile(inputMap.contains)
    val anti2 = LazyList.from(1).map(s => a1 - slope*s).takeWhile(inputMap.contains)

    (anti1 ++ anti2 ++ List(a1, a2)).toSet