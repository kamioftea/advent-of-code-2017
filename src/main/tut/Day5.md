# A Maze of Twisty Trampolines, All Alike

A solution for [Advent of Code 2017 - Day 5](http://adventofcode.com/2017/day/5)

## Part 1

I have a sequence of integers, to be interpreted as jump instructions. Starting 
at the first item, the task is to use the current value of my position as an 
offset to get to a new position. Incrementing the value in the current position
before jumping. I need to return the number of jumps performed before a jump 
would take the current position out of the range of the sequence. 

It seemed best just to simulate the steps. It is useful to note that the 
increment ensures that this will always (eventually) terminate for a finite
sequence.

```tut:book
def countMoves(offsets: Seq[Int]): Int = {
  def iter(os: Vector[Int], count: Int = 0, position: Int = 0): Int =
    if (!os.isDefinedAt(position)) count
    else iter(
      os.updated(position, os(position) + 1),
      count + 1,
      position = position + os(position)
    )
    
  iter(offsets.toVector)
}
``` 

There was only one example provided, but it is fairly simple to synthesise a few
extra tests from very basic examples

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day5Part1Test extends FunSuite with Matchers {
  test("can count jumps") {
    countMoves(Seq(0,3,0,1,-3)) shouldBe 5
    countMoves(Seq(1)) shouldBe 1
    countMoves(Seq(-1)) shouldBe 1
    countMoves(Seq(0)) shouldBe 2
  }
}

(new Day5Part1Test).execute()
```

The actual input was much longer, and presented as one integer per line. The 
simulation was still quick enough not to notice a delay.

```tut:book
import scala.io.Source
def input = Source.fromResource("day5input.txt").getLines().map(_.toInt).toSeq

countMoves(input)
```

# Part 2

This was only a slight modification, now the algorithm converges on 2½. 
Admittedly, it'll never reach this value, instead toggling between 2 and 3. The
algorithm is still guaranteed to terminate as in the worst-case the sequence 
will eventually converge on 2s and 3s, which will shift the pointer off the end.

```tut:book
def countMovesWithConvergence(offsets: Seq[Int]): Int = {
  def iter(os: Vector[Int], count: Int = 0, position: Int = 0): Int =
    if (!os.isDefinedAt(position)) count
    else {
      val offset = os(position)
      iter(
        os.updated(position, if (offset < 3) offset + 1 else offset - 1),
        count + 1,
        position = position + offset
      )
  }
  
  iter(offsets.toVector)
}
```

Most of the tests can be reused, and I added one extra as most of the existing
tests produced the same output despite the change.

```tut:book
class Day5Part2Test extends FunSuite with Matchers {
  test("can count jumps with convergence") {
    countMovesWithConvergence(Seq(0,3,0,1,-3)) shouldBe 10
    countMovesWithConvergence(Seq(1)) shouldBe 1
    countMovesWithConvergence(Seq(-1)) shouldBe 1
    countMovesWithConvergence(Seq(0)) shouldBe 2
    countMovesWithConvergence(Seq(3, 1 , 2, -3)) shouldBe 4
  }
}

(new Day5Part2Test).execute()
```

The change let to increase in jumps required by two orders of magnitude. There 
was about a 4 second delay when running it the first time after compilation.
Given a couple of runs for optimisation, it was running both parts in less
than 1.5 seconds.

```tut:book
 countMovesWithConvergence(input)
```
