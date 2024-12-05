import util.Day
import util.Util._

object Day5 extends Day(5):
  override def solve(): Unit =
    val (ord, upd) = input.split("\n\n").map(_.split("\n")).asTuple
    val orderMap = ord.map(o => o.split("\\|")).groupBy(_.head).view.mapValues(_.map(_(1)).toSet).toMap.withDefaultValue(Set())
    val updates = upd.map(u => u.split(","))

    given Ordering[String] with
      def compare(x: String, y: String): Int = if orderMap(y).contains(x) then 1 else -1

    //Part 1
    println(updates.filter(inOrder).map(l => l(l.length/2).toInt).sum)

    val incorrectlyOrdered = updates.filter(a => !inOrder(a))

    //Part 2
    println(incorrectlyOrdered.map(u => u.sorted).map(l => l(l.length/2).toInt).sum)

  def inOrder(a: Array[String])(using ord: Ordering[String]): Boolean = a.sliding(2).forall(l => ord.lteq(l.head, l(1)))