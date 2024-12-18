import util.{Day, Util}
import util.Util.*

import scala.annotation.tailrec

object Day18 extends Day(18):
  override def solve(): Unit =
    val bytes = inputLines.map(Util.ints).map{case Seq(x, y) => (x, y) }

    val after1Kb = bytes.take(1024).foldLeft(Map[Pos, Char]())((m, p) => m.updated(p, '#')).withDefaultValue('.')

    //Part 1
    println(Util.dijkstra((0, 0), (70, 70), neighbors(after1Kb)).distance)

    //Part 2
    println(findCutoff(after1Kb, 0, bytes.length - 1024, bytes.drop(1024)))

  def neighbors(map: Map[Pos, Char])(p: Pos): List[(Pos, Int)] =
    Util.vonNeumannNeighborhood.map(n => n + p)
      .filter(n => (n.x >= 0 && n.x <= 70) && (n.y >= 0 && n.y <= 70))
      .filter(n => map(n) != '#')
      .map(n => (n, 1))
      .toList

  @tailrec
  def findCutoff(map: Map[Pos, Char], l: Int, u: Int, bytes: List[Pos]): String =
    val mid = (l + u) / 2
    if l == u then
      bytes(mid - 1).x + "," + bytes(mid - 1).y
    else
      val updated = bytes.take(mid).foldLeft(map)((m, p) => m.updated(p, '#'))
      val path = Util.dijkstra((0, 0), (70, 70), neighbors(updated))

      if path.path.isEmpty then
        findCutoff(map, l, mid - 1, bytes)
      else
        findCutoff(map, mid + 1, u, bytes)