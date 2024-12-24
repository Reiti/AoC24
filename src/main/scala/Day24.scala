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

    //Part 2
    println((1 to 44).flatMap(g => findErrors(gates, g)).distinct.sorted.mkString(","))

  def findErrors(gates: List[(String, String)], idx: Int): List[String] =
    val fi = "%02d".format(idx)
    val gate = "z" + fi
    val found = gates.find(_._2 == gate).get

    if !found._1.contains("XOR") then
      val needed1 = ("x" + fi) + " XOR " + ("y" + fi)
      val needed2 = ("y" + fi) + " XOR " + ("x" + fi)
      val corr = gates.find(g => g._1 == needed1 || g._1 == needed2).get._2
      val swap = gates.find(g => g._1.contains("XOR " + corr) || g._1.contains(corr + " XOR")).get
      List(found._2, swap._2)
    else
      val comp = found._1.split(" XOR ")
      val c1 = gates.find(_._2 == comp.head).get
      val c2 = gates.find(_._2 == comp(1)).get

      if !c1._1.contains("XOR") && !c2._1.contains("XOR") then
        val nc = gates.find(g => g._1.contains("XOR") && g._1.contains("x"+fi)).get
        val sw = if c1._1.contains(fi) then c1 else c2
        List(nc._2, sw._2)
      else
        List.empty

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