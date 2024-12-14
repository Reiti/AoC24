import util.{Day, Util}
import util.Util._

import scala.annotation.tailrec

object Day14 extends Day(14):
  override def solve(): Unit =
    val robots = inputLines.map(Util.ints).map{case Seq(px, py, vx, vy) => ((px, py), (vx, vy))}

    //Part 1
    println(safetyFactor(robots.map(r => move(r, 101, 103, 100)._1), 101, 103))

    //Part 2
    println(findTree(robots, 101, 103))

  @tailrec
  def move(r: (Pos, Vel), mx: Int, my: Int, steps: Int): (Pos, Vel) =
    if steps == 0 then
      r
    else
      move(((r._1 + r._2) % (mx, my), r._2), mx, my, steps - 1)

  def safetyFactor(p: List[Pos], mx: Int, my: Int): Int =
    val tl = p.count(r => r.x < mx / 2 && r.y < my / 2)
    val tr = p.count(r => r.x > mx / 2 && r.y < my / 2)
    val bl = p.count(r => r.x < mx / 2 && r.y > my / 2)
    val br = p.count(r => r.x > mx / 2 && r.y > my / 2)

    tl * tr * bl * br

  @tailrec
  def findTree(robots: List[(Pos, Vel)], mx: Int, my: Int, steps: Int = 0): Int =
    if robots.map(_._1).size == robots.map(_._1).toSet.size then
      steps
    else
      findTree(robots.map(r => move(r, mx, my, 1)), mx, my, steps + 1)