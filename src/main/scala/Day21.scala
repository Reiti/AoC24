import util.Util.Pos
import util.{Day, Util}
import util.Util.*

import scala.annotation.tailrec

object Day21 extends Day(21):
  type State = (Pos, String)

  def numpad: Map[Pos, Char] = Util.parseMap(
      """789
        |456
        |123
        |#0A""".stripMargin.linesIterator.toList)

  def dirpad: Map[Pos, Char] =
     Util.parseMap(
       """#^A
         |<v>""".stripMargin.linesIterator.toList)

  override def solve(): Unit =
    //Part 1
    println(inputLines.map(c => complexity(c, 2)).sum)

    //Part 2
    println(inputLines.map(c => complexity(c, 25)).sum)

  def complexity(code: String, intermediaries: Int): Long =
    val onNumpad = ("A" + code).sliding(2).foldLeft(List[String](""))((acc, next) => {
      val r = paths(numpad, next.head, next(1))
      acc.flatMap(c => r.map(v =>  c + v))
    })

    val ls = onNumpad.map(dirPad1Input => ("A" +  dirPad1Input).sliding(2).map(s => sequenceLength(s.head, s(1), intermediaries - 1)).sum)

    val l = ls.min
    val num = code.take(code.length - 1).toLong
    l*num

  def neighbor(keypad: Map[Pos, Char])(p: Pos): List[(Pos, Int)] =
    Util.vonNeumannNeighborhood.toList.map(n => p + n).filter(keypad.contains).filter(n => keypad(n) != '#').map(e => (e, 1))

  def isTarget(keypad: Map[Pos, Char], c: Char)(p: Pos): Boolean = keypad(p) == c

  lazy val sequenceLength: ((Char, Char, Long)) => Long = Util.memoize:
    case (from, to, 0) => paths(dirpad, from, to).map(_.length.toLong).min
    case (from, to, depth) => paths(dirpad, from, to).map(p => "A" + p).map(p => p.sliding(2).map(s => sequenceLength(s.head, s(1), depth - 1)).sum).min

  @tailrec
  def reconstructInputs(path: List[Pos], acc: String = ""): String = path match
    case x :: xs if xs.nonEmpty =>
      val prev = xs.head
      val dir = prev - x

      val next = dir match
        case (0, -1) => "^"
        case (0, 1) => "v"
        case (-1, 0) => "<"
        case (1, 0) => ">"
        case (0, 0) => "A"

      reconstructInputs(xs, acc + next)
    case _ => acc

  def paths(keypad: Map[Pos, Char], start: Char, end: Char): List[String] =
    Util.dijkstraAll(keypad.find(e => e._2 == start).get._1, isTarget(keypad, keypad.find(e => e._2 == end).get._2), neighbor(keypad)).map(p => reconstructInputs(p.path) + "A")