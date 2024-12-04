import util.{Day, Util}

object Day4 extends Day(4):
  override def solve(): Unit =
    val patterns = Util.mooreNeighborhood.map((x, y) => (0 to 3).map(f => (f*x, f*y)).toList)

    //Part 1
    println(inputMap.keys.toList.flatMap(k => patterns.map(p => extract(inputMap.withDefaultValue(' '), p, k))).count(s => s.equals("XMAS")))

    //Part 2
    println(inputMap.keys.count(k => isX(inputMap.withDefaultValue(' '), k)))

  def extract(map: Map[(Int, Int), Char], pattern: List[(Int, Int)], start: (Int, Int)): String =
    pattern.map(pos => map((start._1 + pos._1, start._2 + pos._2))).mkString

  def diag1: List[(Int, Int)] = List((1, 1), (0, 0), (-1, -1))
  def diag2: List[(Int, Int)] = List((-1, 1), (0, 0), (1, -1))

  def isX(map: Map[(Int, Int), Char], pos: (Int, Int)): Boolean =
    val d1 = extract(map, diag1, pos)
    val d2 = extract(map, diag2, pos)

    (d1.equals("MAS") || d1.reverse.equals("MAS")) && (d2.equals("MAS") || d2.reverse.equals("MAS"))