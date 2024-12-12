import util.{Day, Util}
import util.Util._

import scala.annotation.tailrec

object Day12 extends Day(12):
  override def solve(): Unit =
    val reg = regions(inputMap)

    //Part 1
    println(reg.map(r => area(r) * perimeter(r)).sum)

    //Part 2
    println(reg.map(r => area(r) * sides(r)).sum)

  @tailrec
  def regions(map: Map[Pos, Char], found: Seq[Set[Pos]] = Seq()): Seq[Set[Pos]] =
    if map.isEmpty then
      found
    else
      val r = findRegion(map, map.head._1) + map.head._1
      regions(map.filter(p => !r.contains(p._1)), r +: found)

  def findRegion(map: Map[Pos, Char], pos: Pos, found: Set[Pos] = Set()): Set[Pos] =
    vonNeumannNeighborhood
      .filter(p => !found.contains(pos + p) && map.contains(pos + p) && map(pos + p) == map(pos))
      .map(_ + pos)
      .foldLeft(found)((nf, p) => nf | findRegion(map, p, nf + p))

  def area(region: Set[Pos]): Int = region.size

  def perimeter(region: Set[Pos]): Int = region.toList.map(p => 4 - vonNeumannNeighborhood.map(n => n + p).count(region.contains)).sum

  def sides(region: Set[Pos]): Int =
    val scaled = scale(region)
    scaled.count(p => Set(3, 4, 7).contains(mooreNeighborhood.map(n => n + p).count(scaled.contains)))

  def scale(region: Set[Pos]): Set[Pos] = region.map(_ * 3).flatMap(p => Set(p, p + (1, 0), p + (2, 0), p + (0, 1), p + (1, 1), p + (2, 1), p + (0, 2), p + (1, 2), p + (2, 2)))