# Knot Hash

A solution for [Advent of Code 2017 - Day 10](http://adventofcode.com/2017/day/10)

## Part 1

The first part of today's task is to implement a round of a convoluted hashing
algorithm inspired by trying to represent the act of knotting a sequence of 
integers.

My code is mostly just following the recipe provided. It is in two parts as
part two turns out to require re-running the algorithm multiple times, passing
on some of the internal state. The second function is merely a wrapper round
the implementation that formats the output correctly.

```tut:book
def hashRound(lengths: List[Int],
              hash: Vector[Int],
              position: Int = 0,
              skipSize: Int = 0): (Vector[Int], Int, Int) = {
  lengths match {
    case Nil => (hash, position, skipSize)
    case h :: t =>
      val newHash =
        if (position + h < hash.length) {
          val (pre, rest) = hash.splitAt(position)
          val (mid, post) = rest.splitAt(h)

          pre ++ mid.reverse ++ post
        }
        else {
          val (rest, end) = hash.splitAt(position)
          val (start, mid) = rest.splitAt((position + h) % hash.length)
          val (rEnd, rStart) = (end ++ start).reverse.splitAt(end.length)

          rStart ++ mid ++ rEnd
        }

      hashRound(
        t,
        newHash,
        (position + h + skipSize) % hash.length,
        skipSize + 1
      )
  }
}

def hash(lengths: List[Int],
         listSize: Int,
         position: Int = 0,
         skipSize: Int = 0): Vector[Int] =
  hashRound(lengths, Vector.range(0, listSize))._1
``` 

I could have used a single formula to calculate the new position using modulus
calculations to treat is at a continuous list, but it would have been much
harder to understand what it was doing if I need to revisit this code.

There was only one example, but since it is recursive I can use the sub-steps
to add in some extra test cases.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day10Part1Test extends FunSuite with Matchers {
  test("can hash sequence") {
    hash(List(), 5) shouldBe Vector(0, 1, 2, 3, 4)
    hash(List(3), 5) shouldBe Vector(2, 1, 0, 3, 4)
    hash(List(3, 4), 5) shouldBe Vector(4, 3, 0, 1, 2)
    hash(List(3, 4, 1), 5) shouldBe Vector(4, 3, 0, 1, 2)
    hash(List(3, 4, 1, 5), 5) shouldBe Vector(3, 4, 2, 1, 0)
  }
}

(new Day10Part1Test).execute()
```

The input can now be hashed, and the solution required was the first two values
multiplied together.

```tut:book
val input = "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63"
val output = hash(input.split(",").map(_.toInt).toList, 256)
output(0) * output(1)
```
## Part 2

This is quite an extension on the above, and make the output look more like a
hash, even if that is just because it is more efficient to display binary data
as hexadecimal characters so by their nature of having no inherent structure 
themselves, hexadecimal strings is the default output.

Whilst a lot more work is done by the computer, the actual task is still just
following a recipe

```tut:book
def knotHash(input: String, listSize: Int = 256): String = {
  val lengths = input.toList.map(_.toInt) ++ List(17, 31, 73, 47, 23)
  val (sparseHash, _, _) =
    (0 until 64)
      .foldLeft((Vector.range(0, listSize), 0, 0)) {
        case ((h, p, s), _) => hashRound(lengths, h, p, s)
      }

  sparseHash
    .grouped(16)
    .map(_.reduce(_ ^ _))
    .map("%02x".format(_))
    .mkString
}
```

With this part having many more steps a walk-through isn't provided. There are
however a few test cases.

```tut:book
class Day10Part2Test extends FunSuite with Matchers {
  test("can run full knot hash")
  {
    knotHash("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    knotHash("AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    knotHash("1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    knotHash("1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
  }
}

(new Day10Part2Test).execute()
```

The input can now be hashed, giving me my second solution

```tut:book
knotHash(input)
```
