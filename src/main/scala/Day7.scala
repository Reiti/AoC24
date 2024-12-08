import util.Day

object Day7 extends Day(7):
  override def solve(): Unit =
    val r = raw"(\d*):\s((\d*\s?)*)".r
    val parsed = inputLines.map {case r(target, nums, _*) => (BigInt(target), nums.split(" ").toList.map(BigInt.apply))}

    //Part 1
    println(parsed.filter(e => couldBeTrue(e._1, e._2.tail, e._2.head)).map(e => e._1).sum)

    //Part 2
    println(parsed.filter(e => couldBeTrueWithConcat(e._1, e._2.tail, e._2.head)).map(e => e._1).sum)

  def couldBeTrue(target: BigInt, nums: List[BigInt], sum: BigInt = 0): Boolean = nums match
    case x :: xs => sum <= target && couldBeTrue(target, xs, sum + x) || couldBeTrue(target, xs, sum * x)
    case _ => sum == target

  def couldBeTrueWithConcat(target: BigInt, nums: List[BigInt], sum: BigInt = 0): Boolean = nums match
    case x :: xs => sum <= target && couldBeTrueWithConcat(target, xs, sum + x) || couldBeTrueWithConcat(target, xs, sum * x) || couldBeTrueWithConcat(target, xs, BigInt(String.valueOf(sum) + x))
    case _ => sum == target