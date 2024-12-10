import util.{Day, Util}
import util.Util._

object Day10 extends Day(10):
  override def solve(): Unit =
    val trailheads = inputMap.filter(e => e._2 == '0').keys.toList

    //Part 1
    println(trailheads.map(t => distinctPaths(inputMap, t).map(_.head)).map(_.size).sum)

    //Part 2
    println(trailheads.map(t => distinctPaths(inputMap, t)).map(_.size).sum)

  def distinctPaths(map: Map[Pos, Char], pos: Pos, path: Seq[Pos] = Seq.empty): Set[Seq[Pos]] = map.get(pos).map(_.asDigit) match
    case Some(9) => Set(pos +: path)
    case Some(x) => Util.vonNeumannNeighborhood.map(_ + pos).filter(map.contains).filter(p => map(p).asDigit == x + 1).map(p => distinctPaths(map, p, p +: path)).foldLeft(Set())(_ union _)
    case None => Set.empty