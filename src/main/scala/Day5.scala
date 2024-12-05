import util.Day

object Day5 extends Day(5):
  override def solve(): Unit =
    val split = input.split("\n\n")
    val ordering = split.head.split("\n")
    val updates = split(1).split("\n").map(u => u.split(","))
    val orderMap = ordering.map(o => o.split("\\|")).groupBy(_.head).view.mapValues(_.map(_(1)).toSet).toMap

    //Part 1
    println(updates.filter(a => a.sliding(2).forall(l => before(l.head, l(1), orderMap))).map(l => l(l.length/2).toInt).sum)

    val incorrectlyOrdered = updates.filter(a => !a.sliding(2).forall(l => before(l.head, l(1), orderMap)))

    //Part 2
    println(incorrectlyOrdered.map(u => u.sortWith((l, r) => before(l, r, orderMap))).map(l => l(l.length/2).toInt).sum)

  def before(a: String, b: String, orderMap: Map[String, Set[String]]): Boolean =
    if !orderMap.contains(a) then
      false
    else if orderMap.contains(b) && orderMap(b).contains(a) then
      false
    else if orderMap(a).contains(b) then
      true
    else
      orderMap(a).exists(nb => before(b, nb, orderMap))