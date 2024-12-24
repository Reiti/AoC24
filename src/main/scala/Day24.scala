import util.Day

import scala.annotation.tailrec

object Day24 extends Day(24):
  override def solve(): Unit =
    val Array(i, g) = input.split("\n\n")
    val init = i.split("\n").map(s => s.split(": ")).map(s => s.head -> s(1)).toMap
    val gates = g.split("\n").map(s => s.split(" -> ")).map(s => (s.head, s(1))).toList

    val num = calculate(init, gates).filter(e => e._1.startsWith("z")).toList.sortBy(_._1).map(_._2).reverse.mkString

    //Part 1
    println(BigInt(num, 2))

    //Part 2 (found by hand)
    println(List("cbd","gmh","jmq","qrh","rqf","z06","z13","z38").mkString(","))

  @tailrec
  def calculate(values: Map[String, String], gates: List[(String, String)]): Map[String, String] =
    if gates.isEmpty then
      values
    else
      val possible = gates.filter(g => g._1.split("XOR|AND|OR").forall(i => values.contains(i.trim)))
      val rest = gates.filter(g => !possible.contains(g))

      val s = possible.map(g => (g._1.split(" "), g._2)).map(g => ((g._1.head.trim, g._1(1).trim, g._1(2).trim), g._2))

      val mapped = s.map{
        case ((a, "AND", b), t) => if values(a) == "1" && values(b) == "1" then (t, "1") else (t, "0")
        case ((a, "OR", b), t) => if values(a) == "1" || values(b) == "1" then (t, "1") else (t, "0")
        case ((a, "XOR", b), t) => if values(a) != values(b) then (t, "1") else (t, "0")
      }
      calculate(values ++ mapped.toMap, rest)