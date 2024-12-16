import util.{Day, Util}
import util.Util.*

import scala.annotation.tailrec

object Day16 extends Day(16):
  type State = (Pos, Dir)

  override def solve(): Unit =
    val start = (inputMap.find(_._2 == 'S').get._1, Dir.RIGHT)
    val paths = Util.dijkstraAll(start, isEnd(inputMap), neighbors(inputMap))

    //Part 1
    println(paths.head.distance)

    //Part 2
    println(paths.flatMap(r => r.path.map(_._1)).toSet.size)

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