# Disk Defragmentation

A solution for [Advent of Code 2017 - Day 14](http://adventofcode.com/2017/day/14)

## Part 1

For the first part we need to reuse the implementation for [day 10](https://blog.goblinoid.co.uk/aoc-2017-day-10-knot-hash/)

```tut:invisible
object Day10 {

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
}
```
```tut:book
import Day10._
```

With that available I can now use it to run the repeated hash on the input, 
convert it to a binary representation and count the bits that have been set.

```tut:book
def countBits(hexString: String): Int = (
  hexString
    .grouped(4).map(s => Integer.parseInt(s, 16).toBinaryString)
    .mkString("")
    .count(_ == '1')
)

def countUsed(key: String): Int = (
  (0 to 127)
    .map(i => Day10.knotHash(s"$key-$i"))
    .map(countBits)
    .sum
)
```

Check that it works:

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day14Part1Test extends FunSuite with Matchers {

  test("testCountBits") {
    countBits("0") shouldBe 0
    countBits("1") shouldBe 1
    countBits("e") shouldBe 3
    countBits("f") shouldBe 4
    countBits("ff") shouldBe 8
    countBits("1248") shouldBe 4
    countBits("137f") shouldBe 10
    countBits("f0000000000000000000000000000001") shouldBe 5
  }

  test("testCountUsed") {
    countUsed("flqrgnkx") shouldBe 8108
  }
}

(new Day14Part1Test).execute()
```

Run for the puzzle input:

```tut:book
countUsed("xlqgujun")
```

## Part 2

Today's theme seems to be code reuse. This one is slightly less specific but
the challenge allows me to reuse the clustering from 
[day 12](https://blog.goblinoid.co.uk/aoc-2017-day-12-digital-plumber/) if I 
can munge the input into the same input. The only hiccup is that for day 12 it
is assumed that the nodes are sequentially numbered, and have at least one 
connection. For this puzzle we want to ignore the 0 bits. I added a small change
to the algorithm in so that a number of nodes can be excluded from the algorithm
using a map, and build a mask of all the 0 bits as I transform the data into the
expected `Vector[Set[Int]]` format.

```tut:book
import scala.collection.BitSet

def drawBits(hexString: String): String = (
  hexString
    .grouped(4).map(s => Integer.parseInt(s, 16).toBinaryString.formatted("%16s").replaceAll(" ", "0"))
    .mkString("")
)

def clusterWith(root: Int, edges: Vector[Set[Int]]): BitSet = {
  def iter(linkedEdges: Seq[Int], matches: BitSet): BitSet = linkedEdges match {
    case Nil => matches
    case e +: es if matches.contains(e) => iter(es, matches)
    case e +: es => iter(es ++ edges(e), matches + e)
  }
  
  iter(edges(root).toSeq, BitSet(root))
}

def countClusters(edges: Vector[Set[Int]], mask: Vector[Boolean]): Int = {
  def iter(toProcess: Seq[Int], matches: BitSet, count: Int): Int = toProcess match {
    case Nil => count
    case e +: es if matches.contains(e) => iter(es, matches, count)
    case e +: es => iter(es, matches ++ clusterWith(e, edges), count + 1)
  }
  
  iter(edges.indices.filter(e => mask(e)), BitSet.empty, 0)
}

def countRegions(key: String): Int = {
  val matrix = Vector.iterate(0, 128)(_ + 1).map(i => drawBits(Day10.knotHash(s"$key-$i")))
  val edgesWithMask: Vector[(Set[Int], Boolean)] = (for(x <- 0 to 127; y <- 0 to 127) yield {
    (Set((-1, 0), (0, -1), (1, 0), (0,1)).collect {
      case (dx, dy) if matrix.isDefinedAt(x + dx) && matrix(x + dx).isDefinedAt(y + dy) && matrix(x + dx)(y + dy) == matrix(x)(y)
      => y + dy + 128 * (x + dx)
    }, matrix(x)(y) == '1')
  }).toVector

  val edges = edgesWithMask.map(_._1)
  val mask = edgesWithMask.map(_._2)

  countClusters(edges, mask)
}
```

Test that we can generate correct output for the example given

```tut:book
class Day14Part2Test extends FunSuite with Matchers {
  test("testCountRegions") {
    countRegions("flqrgnkx") shouldBe 1242
  }
}

(new Day14Part2Test).execute()
```

With that working I can input the puzzle input and submit a solution

```tut:book
countRegions("xlqgujun")
```
