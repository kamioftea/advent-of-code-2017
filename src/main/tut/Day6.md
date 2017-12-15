# Memory Reallocation

A solution for [Advent of Code 2017 - Day 6](http://adventofcode.com/2017/day/6)

## Part 1

Today I'm tasked with analysing a memory redistribution algorithm. Each 
iteration the algorithm finds the largest int in the sequence and distributes it
as evenly as possible over the whole sequence. To do this it walks the sequence
for n steps (where n is the integer being redistributed) starting at the next
position and cycling if there is overflow.

The value I have to compute is how many iterations of the algorithm are 
performed before a previous sequence state is reached, closing an infinite loop.

The actual redistribution can be reduced to a single loop, using modulus based
calculations to determine the increment for each index.

```tut:book
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

def countUntilLoop(buckets: Vector[Int]): Int = {
  def iter(state: Vector[Int],
           seen: Set[Vector[Int]] = Set.empty,
           count: Int = 0): Int =
    if (seen.contains(state)) count
    else iter(redistribute(state), seen + state, count + 1)

  iter(buckets)
}
```

There was only one value, but due to how it loops, the walkthrough can be used
to synthesise some extra test cases

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day6Part1Test extends FunSuite with Matchers {
  test("can redistribute values") {
    redistribute(Vector(0, 2, 7, 0)) shouldBe Vector(2, 4, 1, 2)
    redistribute(Vector(2, 4, 1, 2)) shouldBe Vector(3, 1, 2, 3)
    redistribute(Vector(23)) shouldBe Vector(23)
    redistribute(Vector(4, 2)) shouldBe Vector(2, 4)
  }
  
  test("can count iterations until repeat") {
    countUntilLoop(Vector(0, 2, 7, 0)) shouldBe 5
    countUntilLoop(Vector(2, 4, 1, 2)) shouldBe 4
    countUntilLoop(Vector(3, 1, 2, 3)) shouldBe 4
    countUntilLoop(Vector(23)) shouldBe 1
    countUntilLoop(Vector(4, 2)) shouldBe 2
    countUntilLoop(Vector(5, 1)) shouldBe 3
  }
}

(new Day6Part1Test).execute()
```

Now I can plug in the input and get a result to claim my gold star.

```tut:book
val input = Vector(0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11)

countUntilLoop(input)
```

## Part 2

This only adds a slight extension to the puzzle, which is to count the length of 
the actual loop, excluding the steps taken before reaching the first position 
that repeats. I could just run the count function again, using the final 
sequence as the initial state would then walk the loop exactly once before
seeing a repeat. Instead to save work, instead of storing a set of sequence 
states, the sequences were used as the keys to a map. The map values being the
iteration that produced the state, meaning I could do a simple subtraction to 
get the loop length.

```tut:book
def countLoopSize(buckets: Vector[Int]): Int = {
  def iter(state: Vector[Int],
           seen: Map[Vector[Int], Int] = Map.empty,
           count: Int = 0): Int =
    if (seen.isDefinedAt(state)) count - seen(state)
    else iter(redistribute(state), seen.updated(state, count), count + 1)

  iter(buckets)
}
``` 

The previous tests could be reworked for the new output

```tut:book
class Day6Part2Test extends FunSuite with Matchers {
  test("testCountLoopSize") {
    countLoopSize(Vector(0, 2, 7, 0)) shouldBe 4
    countLoopSize(Vector(2, 4, 1, 2)) shouldBe 4
    countLoopSize(Vector(3, 1, 2, 3)) shouldBe 4
    countLoopSize(Vector(23)) shouldBe 1
    countLoopSize(Vector(4, 2)) shouldBe 2
    countLoopSize(Vector(5, 1)) shouldBe 2
  }
}

(new Day6Part2Test).execute()
```

I can now claim another gold star

```tut:book
countLoopSize(input)
```
