import util.Day

object Day23 extends Day(23):
  override def solve(): Unit =
    val conns = inputLines.map(_.split("-")).flatMap{case Array(a, b) => List((a, b), (b, a))}.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap

    //Part 1
    println(find3Cliques(conns).count(l => l.exists(s => s.startsWith("t"))))

    //Part 2
    println(conns.keys.map(k => maxClique(conns, Set(k))).maxBy(_.size).toList.sorted.mkString(","))

  def find3Cliques(conns: Map[String, Set[String]]): Set[Set[String]] =
    conns.keys.flatMap(s =>
      conns(s).toList.combinations(2).map(_.toList).filter{ case List(a, b) => conns(a).contains(b) && conns(b).contains(a) }.map(c => Set(c.head, c(1), s))
    ).toSet

  def maxClique(conns: Map[String, Set[String]], acc: Set[String]): Set[String] =
    conns.keys.foldLeft(acc){
      case (connected, next) =>
        if connected.forall(c => conns(c).contains(next) && conns(next).contains(c)) then
          connected + next
        else
          connected
    }