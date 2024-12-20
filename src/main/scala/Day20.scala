import util.{Day, Util}
import util.Util.*

import scala.annotation.tailrec

object Day20 extends Day(20):
  override def solve(): Unit =
    val start = inputMap.find(e => e._2 == 'S').get._1
    val res = Util.dijkstra(start, isEnd(inputMap), neighbor(inputMap))
    val path = res.path.zipWithIndex

    //Part 1
    println(findShortcuts(inputMap, path, 2).size)

    //Part 2
    println(findShortcuts(inputMap, path, 20).size)

  def neighbor(map: Map[Pos, Char])(p: Pos): List[(Pos, Int)] =
    Util.vonNeumannNeighborhood.map(n => n + p).filter(map.contains).filter(n => map(n) != '#').map(n => (n, 1)).toList

  def isEnd(map: Map[Pos, Char])(p: Pos): Boolean = map(p) == 'E'

  @tailrec
  def findShortcuts(map: Map[Pos, Char], remaining: List[(Pos, Int)], cheatDuration: Int, acc: Set[(Pos, Pos)] = Set()): Set[(Pos, Pos)] = remaining match
    case x :: xs =>
      val shortCuts = remaining.filter(p => Util.manhattan(p._1, x._1) <= cheatDuration)
        .filter(n => map(n._1) != '#')
        .filter(r => (r._2 - x._2) >= (100 + Util.manhattan(r._1, x._1)))
        .map(r => (x._1, r._1))
        .toSet
      findShortcuts(map, xs, cheatDuration, acc | shortCuts)
    case _ =>
      acc