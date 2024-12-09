import util.Day

import scala.annotation.tailrec

object Day9 extends Day(9):
  override def solve(): Unit =
    val padded = input.padTo(input.length + 1, '0')
    val withIds = padded.grouped(2).map(s => (s.head.asDigit, s(1).asDigit)).zipWithIndex.toList

    val asDisk = withIds.flatMap(e => List.fill(e._1._1)(e._2.toString) ++ List.fill(e._1._2)("."))

    //Part 1
    println(compactBlocks(asDisk, asDisk.length - 1).zipWithIndex.map((v, i) => if v == "." then 0 else v.toLong*i).sum)

    val withFree = withIds.flatMap(e => List((e._1._1, e._2), (e._1._2, -1)))

    //Part 2
    println(compactFiles(withFree, withFree.map(_._2).filterNot(_ == -1).reverse).flatMap(e => if e._2 == -1 then List.fill(e._1)(".") else List.fill(e._1)(e._2.toString)).zipWithIndex.map((v, i) => if v == "." then 0 else v.toLong*i).sum)

  @tailrec
  def compactBlocks(disk: List[String], cursor: Int): List[String] =
    if disk(cursor) == "." then
      compactBlocks(disk, cursor - 1)
    else
      findFreeBlock(disk, cursor) match
        case Some(idx) => compactBlocks(disk.patch(idx, List(disk(cursor)), 1).patch(cursor, List("."), 1), cursor - 1)
        case None => disk

  def findFreeBlock(disk: List[String], cursor: Int): Option[Int] =
    disk.zipWithIndex.find(e => e._2 < cursor && e._1.equals(".")).map(_._2)

  @tailrec
  def compactFiles(disk: List[(Int, Int)], ids: List[Int]): List[(Int, Int)] = ids match
    case x :: xs =>
      val curr = disk.find(_._2 == x).get
      val idx = disk.indexOf(curr)
      findFreeFile(disk, idx, curr._1) match
        case Some(i) =>
          val (free, _) = disk(i)
          val moved = if curr._1 - free != 0 then disk.patch(i, List((curr._1, curr._2), (free - curr._1, -1)), 1) else disk.patch(i, List((curr._1, curr._2)), 1)
          val toRemove = moved.lastIndexOf(curr)
          val removed = moved.patch(toRemove, List((curr._1, -1)), 1)
          compactFiles(removed, xs)
        case None =>
          compactFiles(disk, xs)

    case _ => disk

  def findFreeFile(disk: List[(Int, Int)], idx: Int, size: Int): Option[Int] =
    val found = disk.zipWithIndex.find(e => e._2 < idx && e._1._2 == -1 && e._1._1 >= size).map(_._1)
    found.map(f => disk.indexOf(f))