# Packet Scanners

A solution for [Advent of Code 2017 - Day 13](http://adventofcode.com/2017/day/13)

## Part 1

The input is fairly easy to get into a useful form. 

```tut:book
case class Layer(depth: Int, range: Int)

val LineMatcher = "(\\d+): (\\d+)".r

def parseLines(lines: TraversableOnce[String]): Seq[Layer] =
  lines.toSeq.collect { case LineMatcher(d, r) => Layer(d.toInt, r.toInt) }
```

I briefly considered simulating the 'firewall', but I realised that each 
'security scanner' will be at position 0 every `2 x Layer.range - 2` seconds. 
Since we will be at each layer at `Layer.depth` seconds, I can calculate if a
bot will detect me with a single calculation for its layer. The full solution
is easy to express as a recipe for the collections library.

```tut:book
def calcTripSeverity(layers: Seq[Layer]): Int = (
  layers
    .filter(l => l.depth % (2 * l.range - 2) == 0)
    .map(l => l.depth * l.range)
    .sum
)
```

This can now be tested...

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day13Part1Test extends FunSuite with Matchers {

  test("testParseLines") {
    parseLines(
      """0: 3
        |1: 2
        |4: 4
        |6: 4
      """.stripMargin.lines
    ) shouldBe Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 4),
      Layer(6, 4)
    )
  }

  test("testCalcTripSeverity") {
    calcTripSeverity(Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 4),
      Layer(6, 4)
    )) shouldBe 24

    calcTripSeverity(Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 3),
      Layer(5, 7),
      Layer(6, 4)
    )) shouldBe 36
  }
}

(new Day13Part1Test).execute()
```
... and run.
```tut:book
import scala.io.Source
def input = parseLines(Source.fromResource("day13input.txt").getLines())

calcTripSeverity(input)
```

## Part 2

This required finding a safe time-slice to cross the 'firewall'. I initially
tried to reuse part 1 a bit more in this, but was caught out by the scanner in 
layer 0 detecting me, even though it added 0 severity. In the end it was easier
to write a new recipe with the same check.

```tut:book
def calcSafeTrip(layers: Seq[Layer]): Int = (
  Stream.from(0)
    .filter(offset =>
      layers.forall(l =>
        (l.depth + offset) % (2 * l.range - 2) != 0
      )
    )
    .head
  )
```

This can then be tested with the example given

```tut:book
class Day13Part2Test extends FunSuite with Matchers {
  test("testCalcSafeTrip") {
    calcSafeTrip(Seq(
      Layer(0, 3),
      Layer(1, 2),
      Layer(4, 4),
      Layer(6, 4)
    )) shouldBe 10
  }
}

(new Day13Part2Test).execute()
```

The code can now be run. Despite the large delay, the solution takes roughly 2 
seconds to run both parts.

```tut:book
calcSafeTrip(input)
```
