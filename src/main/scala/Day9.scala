import util.Day

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

object Day9 extends Day(9):
  sealed trait Region
  sealed case class Free(size: Int) extends Region
  sealed case class File(size: Int, id: Int) extends Region

  override def solve(): Unit =
    val regions = input.grouped(2).zipWithIndex.flatMap((l, i) => List(File(l.head.asDigit, i), Free(l.lastOption.map(_.asDigit).getOrElse(0)))).toVector

    //Part 1
    println(checksum(compactBlocks(regions)))

    //Part 2
    println(checksum(compactFiles(regions)))

  @tailrec
  def compactBlocks(regions: Vector[Region], done: Vector[Region] = Vector()): Vector[Region] = regions match
    case beginning :+ Free(_) => compactBlocks(beginning, done)
    case (x: File) +: tail => compactBlocks(tail, done :+ x)
    case (head: Free) +: mid :+ (last: File) =>
      if head.size == last.size then compactBlocks(mid, done :+ last)
      else if head.size < last.size then compactBlocks(mid :+ File(last.size - head.size, last.id), done :+ File(head.size, last.id))
      else compactBlocks(Free(head.size - last.size) +: mid, done :+ File(last.size, last.id))
    case _ => done

  @tailrec
  def compactFiles(regions: Vector[Region], done: Vector[Region] = Vector(), moved: Set[Int] = Set()): Vector[Region] = regions match
    case beginning :+ (last: Free) => compactFiles(beginning, last +: done, moved)
    case beginning :+ (last: File) if moved.contains(last.id) => compactFiles(beginning, last +: done, moved)
    case beginning :+ (last: File) => findFree(beginning, last) match
      case Some((free, idx)) => compactFiles(beginning.patch(idx, createPatch(free, last), 1) :+ Free(last.size), done, moved + last.id)
      case None => compactFiles(beginning, last +: done, moved + last.id)
    case _ => done

  def createPatch(free: Free, file: File): List[Region] = List(File(file.size, file.id), Free(free.size - file.size))

  def findFree(regions: Vector[Region], curr: File): Option[(Free, Int)] = regions.zipWithIndex.collectFirst { case (f: Free, idx: Int) if f.size >= curr.size => (f, idx)}

  @tailrec
  def checksum(regions: Vector[Region], idx: Int = 0, acc: Long = 0L): Long = regions match
    case Free(size) +: xs => checksum(xs, idx + size, acc)
    case File(size, id) +: xs => checksum(xs, idx + size, acc + idx.until(idx + size).map(i => i.toLong*id).sum)
    case _ => acc