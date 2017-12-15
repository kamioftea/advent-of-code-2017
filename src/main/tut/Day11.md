# Corruption Checksum

A solution for [Advent of Code 2017 - Day 11](http://adventofcode.com/2017/day/11)

## Part 1

I need to find the shortest path to the origin on an infinite hex grid after 
following a sequence of directions. So I set up some representations of a 
coordinate and a direction.

```tut:book
case class Coordinate(x: Int, y: Int, z: Int) 

sealed trait Direction {
  def transform (coordinate: Coordinate): Coordinate
}

case object N extends Direction {
  override def transform(c: Coordinate): Coordinate = 
    c.copy(x = c.x + 1, z = c.z - 1)
}

case object NE extends Direction {
  override def transform(c: Coordinate): Coordinate = 
    c.copy(x = c.x + 1, y = c.y - 1)
}

case object SE extends Direction {
  override def transform(c: Coordinate): Coordinate = 
    c.copy(y = c.y - 1, z = c.z + 1)
}

case object S extends Direction {
  override def transform(c: Coordinate): Coordinate = 
    c.copy(x = c.x - 1, z = c.z + 1)
}

case object SW extends Direction {
  override def transform(c: Coordinate): Coordinate = 
    c.copy(x = c.x - 1, y = c.y + 1)
}

case object NW extends Direction {
  override def transform(c: Coordinate): Coordinate = 
    c.copy(y = c.y + 1, z = c.z - 1)
}
```

Parsing the input is just mapping strings to the relevant case object:

```tut:book
def parsePath(path: String): Seq[Direction] = 
  path.split(',').toSeq.collect {
    case "n" => N
    case "ne" => NE
    case "se" => SE
    case "s" => S
    case "sw" => SW
    case "nw" => NW
  }
```

Now I can actually write some logic. I must admit I didn't really have an
intuition for this problem, but I found a random blog post that helped, 
[Hexagon grids: coordinate systems and distance calculations](http://keekerdc.com/2011/03/hexagon-grids-coordinate-systems-and-distance-calculations/)

```tut:book

def distanceBetween(a: Coordinate, b: Coordinate): Int =
  Seq(Math.abs(a.x - b.x), Math.abs(a.y - b.y), Math.abs(a.z - b.z)).max
  
val origin = Coordinate(0, 0, 0)

def homeDistance(path: Seq[Direction]): Int = {
  val destination = path.foldLeft(origin) {
    case (c, d) => d.transform(c)
  }
  
  distanceBetween(destination, origin)
}

import org.scalatest.{FunSuite, Matchers}

class Day11Part1Test extends FunSuite with Matchers {
  test("Can find distance from origin") {
    homeDistance(parsePath("ne,ne,ne")) shouldBe 3
    homeDistance(parsePath("ne,ne,sw,sw")) shouldBe 0
    homeDistance(parsePath("ne,ne,s,s")) shouldBe 2
    homeDistance(parsePath("ne,n,nw")) shouldBe 2
    homeDistance(parsePath("se,sw,se,sw,sw")) shouldBe 3
  }
}

(new Day11Part1Test).execute()
``` 

I'm now able to find a solution for the puzzle input

```tut:book
import scala.io.Source

val puzzlePath = parsePath(
  Source.fromResource("day11input.txt").mkString("").trim
)

homeDistance(puzzlePath)
``` 
## Part 2 

This is much easier, given part one. I now need to find the maximum distance 
from the origin at any point along the path. This can be done just composing 
methods from the Scala collections library. 

```tut:book
def maxDistance(path: Seq[Direction]): Int = {
  val breadcrumbs = path.scanLeft(origin) {
    case (c, d) => d.transform(c)
  }

  breadcrumbs.map(c => distanceBetween(c, origin)).max
}

```

Which can be tested and then run.

````tut:book

class Day11Part2Test extends FunSuite with Matchers {
  test("Can find max distance from origin") {
    maxDistance(parsePath("ne,ne,ne")) shouldBe 3
    maxDistance(parsePath("ne,ne,sw,sw")) shouldBe 2
    maxDistance(parsePath("ne,ne,sw,sw,se,se,se,nw,nw,nw")) shouldBe 3
    maxDistance(parsePath("ne,ne,s,s")) shouldBe 2
    maxDistance(parsePath("ne,n,nw")) shouldBe 2
    maxDistance(parsePath("se,sw,se,sw,sw")) shouldBe 3
  }
}

(new Day11Part2Test).execute()

maxDistance(puzzlePath)
```
