import util.{Day, Util}
import util.Util.*

import scala.annotation.tailrec

object Day16 extends Day(16):
  type State = (Pos, Dir)

  override def solve(): Unit =
    val start = (inputMap.find(_._2 == 'S').get._1, Dir.RIGHT)
    val optimalScore = Util.dijkstra(start, isEnd(inputMap), neighbors(inputMap)).distance

    //Part 1
    println(optimalScore)

    //Part 2
    println(findTilesBfs(inputMap, optimalScore, List(List((start._1, start._2, 0)))).size)

  @tailrec
  def findTilesBfs(map: Grid, optimalCost: Int, paths: List[List[(Pos, Dir, Int)]], visited: Map[(Pos, Dir), Int] = Map.empty): Set[Pos] =
    val newPaths = paths.flatMap(pa => {
      step(map, pa.last).map(pa.appended)
    }).filter(l => l.last._3 <= optimalCost && !(visited.contains((l.last._1, l.last._2)) && visited((l.last._1, l.last._2)) <= l.last._3) )

    if newPaths.isEmpty then
      paths.filter(l => map(l.last._1) == 'E').flatMap(_.map(_._1)).toSet
    else
      findTilesBfs(map, optimalCost, newPaths, visited ++ newPaths.map(_.last).map(e => (e._1, e._2) -> e._3))

  def step(map: Grid, from: (Pos, Dir, Int)): List[(Pos, Dir, Int)] = map(from._1 + from._2) match
    case '#' => List((from._1, from._2.clockwise, from._3 + 1000), (from._1, from._2.counterclockwise, from._3 + 1000))
    case _ => List((from._1, from._2.clockwise, from._3 + 1000), (from._1, from._2.counterclockwise, from._3 + 1000), (from._1 + from._2, from._2, from._3 + 1))

  def neighbors(map: Grid)(s: State): List[(State, Int)] = s match
    case (p, d) => List(((p, d.clockwise), 1000), ((p, d.counterclockwise), 1000)) ++ map.get(p + d).map {
      case '#' => List.empty
      case _ => List(((p + d, d), 1))
    }.getOrElse(List.empty)

  def isEnd(map: Grid)(s: State): Boolean = map.get(s._1) match
    case Some(c) => c == 'E'
    case _ => false