import util.Day
import util.Util._
import scala.annotation.tailrec

object Day6 extends Day(6):
  override def solve(): Unit =
    val start = inputMap.find(e => e._2 == '^').get._1
    val uniqueTiles = walk(inputMap, start, Dir.UP)._1.map(_._1)

    //Part 1
    println(uniqueTiles.size)
    
    //Part 2
    println(uniqueTiles.filter(t => t != start).count(t => walk(inputMap + (t -> '#'), start, Dir.UP)._2))

  @tailrec
  def walk(map: Map[Pos, Char], pos: Pos, dir: Dir, visited: Set[(Pos, Dir)] = Set()): (Set[(Pos, Dir)], Boolean) =
    if visited.contains((pos, dir)) then
      (visited, true)
    else
      map.get(pos + dir) match
        case Some(c) =>
          if c == '#' then
            walk(map, pos, dir.clockwise, visited)
          else
            walk(map, pos + dir, dir, visited + ((pos, dir)))
            
        case None => (visited + ((pos, dir)), false)