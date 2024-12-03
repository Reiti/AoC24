import util.Day

object Day3 extends Day(3):
  override def solve(): Unit =
    val mul = """mul\((\d+),(\d+)\)""".r

    //Part 1
    println(mul.findAllMatchIn(input).map(m => m.subgroups.map(_.toInt).product).sum)

    //Part 2
    println(mul.findAllMatchIn(input.replaceAll("""(?s)don't\(\)(.*?)do\(\)""", "")).map(m => m.subgroups.map(_.toInt).product).sum)