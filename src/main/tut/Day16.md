# Permutation Promenade

A solution for [Advent of Code 2017 - Day 16](http://adventofcode.com/2017/day/16)

## Part 1

Part one is effectively just parsing the input.

```tut:book
val Spin = "s(\\d+)".r
val Exchange = "x(\\d+)/(\\d+)".r
val Partner = "p([a-z])/([a-z])".r

def applyInstruction(instruction: String, programs: Vector[Char]): Vector[Char] =
  instruction match {
    case Spin(n) => programs.takeRight(n.toInt) ++ programs.dropRight(n.toInt)
    case Exchange(a,b) =>
      programs
        .updated(a.toInt, programs(b.toInt))
        .updated(b.toInt, programs(a.toInt))
    case Partner(a, b) =>
      programs
          .updated(programs.indexOf(a.charAt(0)), b.charAt(0))
          .updated(programs.indexOf(b.charAt(0)), a.charAt(0))

  }
  
def applyInstructions(instructions: Array[String], programs: Vector[Char]): Vector[Char] =
  instructions.foldLeft(programs){ case( ps, i) => applyInstruction(i, ps) }
```

This assumes the input string has already been split on commas. The examples 
given in the instructions convert nicely to test cases.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day16Part1Test extends FunSuite with Matchers {

  test("can apply an instruction") {
    applyInstruction("s1", "abcde".toVector) shouldBe "eabcd".toVector
    applyInstruction("x3/4", "eabcd".toVector) shouldBe "eabdc".toVector
    applyInstruction("pe/b", "eabdc".toVector) shouldBe "baedc".toVector
  }

  test("can apply program") {
    applyInstructions("s1,x3/4,pe/b".split(','), "abcde".toVector) shouldBe "baedc".toVector
  }
}

(new Day16Part1Test).execute()
```

Running with the puzzle input gives the correct output.

```tut:book
import scala.io.Source
val input = Source.fromResource("day16input.txt").mkString.trim.split(',')

applyInstructions(input, "abcdefghijklmnop".toVector).mkString
```

## Part 2

Part two is a bit trickier. I must admit I tried just running the program a
billion times, but it was going nowhere fast. I then noted that hopefully the
output would eventually repeat. I already had a function that could find the
loop size of a repeating program. I needed to make it work with strings, and
since I was reusing it I made it generic.

```tut:book
def countLoopSize[T](sequence: Vector[T], update: Vector[T] => Vector[T]): (Int, Int, Map[Vector[T], Int]) = {
  def iter(state: Vector[T],
           seen: Map[Vector[T], Int] = Map.empty,
           count: Int = 0): (Int, Int, Map[Vector[T], Int]) =
    if (seen.isDefinedAt(state)) (count, count - seen(state), seen)
    else iter(update(state), seen.updated(state, count), count + 1)
    
  iter(sequence)
}
```

I could then use the output from that to work out at what point round the loop
the programs would be after 1 billion iterations.

```tut:book
def applyRepeatInstructions(n: Int, instructions: Array[String], programs: Vector[Char]): Vector[Char] = {
  val (count, loopSize, seen) = countLoopSize(programs, (ps:Vector[Char]) => applyInstructions(instructions, ps))
  val prefix = count - loopSize

  seen.find { case (_, i) => i == prefix + ((n - prefix) % loopSize)}.get._1
}
```

I don't really have sample looping data to test this scenario, but I can ensure
the refactor of countLoopSize works by running the Day 6 test case through the
new function.

```tut:invisible
object Day6 {
  def redistribute(buckets: Vector[Int]): Vector[Int] = {
    val max = buckets.max
    val source = buckets.indexOf(max)
    val start = (source + 1) % buckets.length
    val finish = (start + (max % buckets.length)) % buckets.length
    val shouldGetMore: Int => Boolean =
      if (start <= finish) i => i >= start && i < finish
      else i => i >= start || i < finish
    
    buckets
      .updated(source, 0)
      .zipWithIndex
      .map {
        case (b, i) =>
          b + (max / buckets.length) + (if (shouldGetMore(i)) 1 else 0)
      }
  }
}
```
```tut:book
class Day16Part2Test extends FunSuite with Matchers {
  test("refactor count loop size") {
      countLoopSize(Vector(0, 2, 7, 0), Day6.redistribute)._2 shouldBe 4
      countLoopSize(Vector(2, 4, 1, 2), Day6.redistribute)._2 shouldBe 4
      countLoopSize(Vector(3, 1, 2, 3), Day6.redistribute)._2 shouldBe 4
      countLoopSize(Vector(23), Day6.redistribute)._2 shouldBe 1
      countLoopSize(Vector(4, 2), Day6.redistribute)._2 shouldBe 2
      countLoopSize(Vector(5, 1), Day6.redistribute)._2 shouldBe 2
  }
}

(new Day16Part2Test).execute()
```

Now I run today's input through the loop counter and hope that it does in fact
repeat (spoiler: it does).

```tut:book
applyRepeatInstructions(1000000000, input, "abcdefghijklmnop".toVector).mkString
```
