import util.Day

import scala.annotation.tailrec

object Day22 extends Day(22):
  override def solve(): Unit =
    val numbers = inputLines.map(_.toLong).map(n => evolve(n, 2000))

    //Part 1
    println(numbers.map(_.head).sum)

    val allHistories = numbers.map(n => priceAndHistory(n))
    val allSequences = allHistories.flatMap(n => n.keys).toSet

    //Part 2
    println(allSequences.map(s => price(allHistories, s)).max)

  def price(histories: List[Map[List[Int], Int]], history: List[Int]): Int =
    histories.map(h => h.getOrElse(history, 0)).sum

  @tailrec
  def evolve(num: Long, steps: Int, acc: List[Long] = List.empty): List[Long] =
    if steps == 0 then
      acc
    else
      val n = step(num)
      evolve(n, steps - 1, acc.prepended(n))

  def step(num: Long): Long =
    val s = (num ^ (num << 6)) % 16777216L
    val s1 = ((s >> 5) ^ s) % 16777216L
    ((s1 << 11) ^ s1) % 16777216L

  def priceAndHistory(nums: List[Long]): Map[List[Int], Int] =
    val r = nums.reverse.sliding(5).map(l => ((l.last % 10).toInt, l.sliding(2).map(p => (p.last % 10).toInt - (p.head % 10).toInt).toList)).toList
    r.groupBy(_._2).view.mapValues(_.head._1).toMap