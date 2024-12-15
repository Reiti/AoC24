import util.{Day, Util}
import util.Util.*

import scala.annotation.tailrec

object Day15 extends Day(15):
  override def solve(): Unit =
    val Array(m, i) = input.split("\n\n")
    val map = Util.parseMap(m.split("\n")).withDefaultValue('.')
    val ins = i.replace("\n", "").map{
      case '>' => Dir.RIGHT
      case '^' => Dir.UP
      case '<' => Dir.LEFT
      case 'v' => Dir.DOWN
    }.toList

    val r = map.find(e => e._2 == '@').get._1

    //Part 1
    println(gps(move(map, r, ins, step)))

    val scaledMap = scaleUp(map).withDefaultValue('.')
    val scaledR = scaledMap.find(e => e._2 == '@').get._1

    //Part 2
    println(gpsWide(move(scaledMap, scaledR, ins, stepWide)))

  def move(map: Grid, p: Pos, ins: List[Dir], s: (Grid, Pos, Dir) => (Grid, Pos)): Grid =
    ins.foldLeft((map, p)){ case ((m, p), n) => s(m, p, n) }._1

  def step(map: Grid, p: Pos, d: Dir): (Grid, Pos) = map(p + d) match
    case '#' => (map, p)
    case '.' => (map.updated(p, '.') + ((p + d) -> '@'), p + d)
    case 'O' => findBoxes(map, p + d, d) match
      case Some(boxes) =>
        val shifted = boxes.map(s => s + d)
        (shifted.foldLeft(map.removedAll(boxes).removed(p).updated(p + d, '@'))((m, n) => m.updated(n, 'O')), p + d)
      case None => (map, p)

  @tailrec
  def findBoxes(map: Grid, p: Pos, d: Dir, acc: Set[Pos] = Set.empty): Option[Set[Pos]] = map(p) match
    case '#' => None
    case '.' => Some(acc)
    case 'O' => findBoxes(map, p + d, d, acc + p)

  def gps(map: Grid): Int = map.filter(_._2 == 'O').keys.map((x, y) => x + 100 * y).sum

  def stepWide(map: Grid, p: Pos, d: Dir): (Grid, Pos) = map(p + d) match
    case '#' => (map, p)
    case '.' => (map.updated(p, '.') + ((p + d) -> '@'), p + d)
    case '[' | ']' => findBoxesWide(map, p + d, d) match
      case Some(boxes) =>
        val shifted = boxes.map(s => s + d)
        (shifted.foldLeft(map.removedAll(boxes).removed(p).updated(p + d, '@'))((m, n) => m.updated(n, map(n - d))), p + d)
      case None => (map, p)

  def findBoxesWide(map: Grid, p: Pos, d: Dir, acc: Set[Pos] = Set.empty): Option[Set[Pos]] = d match
    case Dir.LEFT | Dir.RIGHT => map(p) match
      case '#' => None
      case '.' => Some(acc)
      case '[' | ']' => findBoxesWide(map, p + d, d, acc + p)
    case _ => map(p) match
      case '#' => None
      case '.' => Some(acc)
      case '[' =>
        val l = findBoxesWide(map, p + d, d, acc + p)
        val r = findBoxesWide(map, (p + Dir.RIGHT) + d, d, acc + (p + Dir.RIGHT))

        if l.isDefined && r.isDefined then
          Some(l.get | r.get)
        else None
      case ']' =>
        val r = findBoxesWide(map, p + d, d, acc + p)
        val l = findBoxesWide(map, (p + Dir.LEFT) + d, d, acc + (p + Dir.LEFT))

        if l.isDefined && r.isDefined then
          Some(l.get | r.get)
        else None

  def gpsWide(map: Grid): Int = map.filter(_._2 == '[').keys.map((x, y) => x + 100 * y).sum

  def scaleUp(map: Grid): Grid =
    map.map(e => ((e._1.x * 2, e._1.y), e._2)).flatMap{
      case e @ (p, '.') => Set(e, (p.x + 1, p.y) -> '.')
      case e @ (p, '#') => Set(e, (p.x + 1, p.y) -> '#')
      case e @ (p, '@') => Set(e, (p.x + 1, p.y) -> '.')
      case (p, 'O') => Set(p -> '[', (p.x + 1, p.y) -> ']')
    }