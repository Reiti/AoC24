import util.Day

import scala.annotation.tailrec

object Day6 extends Day(6):
  extension (c: (Int, Int))
    def +(o: (Int, Int)): (Int, Int) = (c._1 + o._1, c._2 + o._2)

  override def solve(): Unit =
    val start = inputMap.find(e => e._2 == '^').get._1

    val uniqueTiles = walk(inputMap, start, (0, -1))._1.map(_._1)

    //Part 1
    println(uniqueTiles.size)

    //Part 2
    println(uniqueTiles.filter(t => t != start).count(t => walk(inputMap + (t -> '#'), start, (0, -1))._2))

  @tailrec
  def walk(map: Map[(Int, Int), Char], pos: (Int, Int), dir: (Int, Int), visited: Set[((Int, Int), (Int, Int))] = Set()): (Set[((Int, Int), (Int, Int))], Boolean) =
    if visited.contains((pos, dir)) then
      (visited, true)
    else
      map.get(pos + dir) match
        case Some(c) =>
          if c == '#' then
            walk(map, pos, (-dir._2, dir._1), visited)
          else
            walk(map, pos + dir, dir, visited + ((pos, dir)))

        case None => (visited + ((pos, dir)), false)