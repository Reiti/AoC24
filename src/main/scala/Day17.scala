import util.Day

import scala.annotation.tailrec

object Day17 extends Day(17):
  override def solve(): Unit =
    val Array(r, p) = input.split("\n\n")
    val registers = raw"(\d+)".r.findAllIn(r).map(_.toLong).toArray
    val program = raw"(\d+)".r.findAllIn(p).map(_.toLong).toArray

    //Part 1
    println(execute(program, 0, registers))

    //Part 2
    println(BigInt(constructSolution(program, 4, "2,4,1,6,7,5,4,4,1,7,0,3,5,5,3,0").head, 8))

  @tailrec
  def constructSolution(program: Array[Long], currLength: Int, target: String, acc: List[String] = List("0")): List[String] =
    if currLength > program.length then
      acc
    else
      val sols = for(s <- acc; i <- BigInt(s + "0000", 8) until BigInt(s + "7777", 8) if execute(program, 0, Array(i.toLong, 0, 0)).endsWith(target.takeRight(currLength * 2 - 1))) yield i
      constructSolution(program, currLength + 4, target, sols.map(_.toString(8)))

  @tailrec
  def execute(p: Array[Long], i: Int, reg: Array[Long], o: Vector[Long] = Vector.empty): String =
    p.lift(i) match
      case Some(v) => v match
        case 0 =>
          val n = reg(0)
          val d = Math.pow(2, getCombo(p(i + 1), reg)).toLong
          execute(p, i + 2, reg.updated(0, n/d), o)
        case 1 =>
          execute(p, i + 2, reg.updated(1, reg(1) ^ p(i + 1)), o)
        case 2 =>
          execute(p, i + 2, reg.updated(1, getCombo(p(i + 1), reg) % 8L), o)
        case 3 =>
          if reg(0) == 0 then
            execute(p, i + 2, reg, o)
          else
            execute(p, p(i + 1).toInt, reg, o)
        case 4 =>
          execute(p, i + 2, reg.updated(1, reg(1) ^ reg(2)), o)
        case 5 =>
          execute(p, i + 2, reg, o :+ (getCombo(p(i + 1), reg) % 8L))
        case 6 =>
          val n = reg(0)
          val d = Math.pow(2, getCombo(p(i + 1), reg)).toInt
          execute(p, i + 2, reg.updated(1, n/d), o)
        case 7 =>
          val n = reg(0)
          val d = Math.pow(2, getCombo(p(i + 1), reg)).toInt
          execute(p, i + 2, reg.updated(2, n/d), o)
      case None => o.mkString(",")

  def getCombo(v: Long, reg: Array[Long]): Long = v match
    case 0 | 1 | 2 | 3 => v
    case 4 => reg(0)
    case 5 => reg(1)
    case 6 => reg(2)