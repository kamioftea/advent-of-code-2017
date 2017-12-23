# Dueling Generators

A solution for [Advent of Code 2017 - Day 15](http://adventofcode.com/2017/day/15)

## Part 1

I took a while trying to generate the sequences using scala streams, but there
seems to be no easy way to make them tail recursive, even though their laziness
is the main feature. I then discovered Iterator.iterate which does exactly what
I want. 

```tut:book
def buildIterator(start: Int, factor: Long): Iterator[Int] =
  Iterator.iterate(start)(i => ((i * factor) % 2147483647).toInt).drop(1)
```

I made the least significant bit comparator take in the number of bits required
so that it was easier to write extra test cases,

```tut:book
def buildLSBComparator(numBits: Int): ((Int, Int)) => Boolean = {
  val mask = (1 << numBits) - 1

  (p: (Int, Int)) => (p._1 & mask) == (p._2 & mask)
}
```

Finally given two iterators, I need to be able to count the number that match
for a given number of iterations.

```tut:book
def countMatches(a: Iterator[Int], b: Iterator[Int], iterations: Int): Int = {
  a.take(iterations)
    .zip(b.take(iterations))
    .count(buildLSBComparator(16))
}
```

Tests for all of the above can be built from the puzzle description

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day15Part1Test extends FunSuite with Matchers {

  test("can build generator") {
    buildIterator(65, 16807).take(5).toSeq shouldBe Seq(
      1092455,
      1181022009,
      245556042,
      1744312007,
      1352636452
    )

    buildIterator(8921, 48271).take(5).toSeq shouldBe Seq(
      430625591,
      1233683848,
      1431495498,
      137874439,
      285222916
    )
  }

  test("can compare bits") {
    buildLSBComparator(5)((1092455, 430625591)) shouldBe false
    buildLSBComparator(5)((1181022009, 1233683848)) shouldBe false
    buildLSBComparator(5)((245556042, 1431495498)) shouldBe true
    buildLSBComparator(5)((1744312007, 137874439)) shouldBe true
    buildLSBComparator(5)((1352636452, 285222916)) shouldBe true

    buildLSBComparator(16)((1092455, 430625591)) shouldBe false
    buildLSBComparator(16)((1181022009, 1233683848)) shouldBe false
    buildLSBComparator(16)((245556042, 1431495498)) shouldBe true
    buildLSBComparator(16)((1744312007, 137874439)) shouldBe false
    buildLSBComparator(16)((1352636452, 285222916)) shouldBe false
  }

  test("can count matches") {
    countMatches(
      buildIterator(65, 16807),
      buildIterator(8921, 48271),
      5
    ) shouldBe 1

    countMatches(
      buildIterator(65, 16807),
      buildIterator(8921, 48271),
      40000000
    ) shouldBe 588
  }
}

(new Day15Part1Test).execute()
```

All looks good. I can now run this for my puzzle input.

```tut:book
countMatches(
  buildIterator(591, 16807),
  buildIterator(393, 48271),
  40000000
)
```

## Part 2

Part 2 just involved filtering out values that are not divisible by a certain 
factor.

```tut:book
def filterIterator(start: Int, factor: Long, divisor: Int): Iterator[Int] =
  buildIterator(start, factor).filter(_ % divisor == 0)
```

Which is simple enough, but I'll test it anyway since there were examples 
provided.

```tut:book
class Day15Part2Test extends FunSuite with Matchers {
  test("can filter results") {
    filterIterator(65, 16807, 4).take(5).toSeq shouldBe Seq (
      1352636452,
      1992081072,
      530830436,
      1980017072,
      740335192
    )

    filterIterator(8921, 48271, 8).take(5).toSeq shouldBe Seq (
      1233683848,
      862516352,
      1159784568,
      1616057672,
      412269392
    )
  }
}

(new Day15Part2Test).execute()
```

I now need to refactor my above solution to use the filters and reduced 
iteration count.

```tut:book
countMatches(
  filterIterator(591, 16807, 4),
  filterIterator(393, 48271, 8),
  5000000
)
````
