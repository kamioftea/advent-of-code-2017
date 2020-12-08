# A Series of Tubes

A solution for [Advent of Code 2017 - Day 19](http://adventofcode.com/2017/day/19)

## Part 1

Since all I needed was a grid of characters that I could access by index, then
just parsing the input into a `Vector[String]` is good enough.

To find the answer, I need to find where the path starts and then follow it 
until the next step is not valid. For `-` and `|` I can just step forward, and
disregarding which one I have seen means that the points where a line crosses 
are handled correctly. Letters are pretty much the same, but need to be 
recorded, finally only `+` can change direction, so on finding one we need to
check all three possible next steps for a valid character.


```tut:book
def findStart(grid: Vector[String]): Int = {
  grid(0).indexOf('|')
}

def findPath(grid: Vector[String]): (String, Int) = {
  def canTravel(x: Int, y: Int): Boolean =
    grid.isDefinedAt(y) &&
      grid(y).isDefinedAt(x) &&
      grid(y)(x) != ' '

  def init(x: Int, y: Int, path: String, dx: Int, dy: Int, count: Int): (String, Int) =
    if (!canTravel(x, y))
      (path, count)
    else grid(y)(x) match {
      case '|' | '-' => init(x + dx, y + dy, path, dx, dy, count + 1)
      case '+' =>
        if (canTravel(x + dx, y + dy)) init(x + dx, y + dy, path, dx, dy, count + 1)
        else if (canTravel(x + dy, y + dx)) init(x + dy, y + dx, path, dy, dx, count + 1)
        else if (canTravel(x - dy, y - dx)) init(x - dy, y - dx, path, -dy, -dx, count + 1)
        else (path, count)
      case c => init(x + dx, y + dy, path + c, dx, dy, count + 1)
    }

  init(findStart(grid), 0, "", 0, 1, 0)
}
```

There is a sample mini-grid provided which can be used to make test cases.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day19Part1Test extends FunSuite with Matchers {

  private val testGrid =
    """#     |
       #     |  +--+
       #     A  |  C
       # F---|----E|--+
       #     |  |  |  D
       #     +B-+  +--+""".stripMargin('#').lines.toVector

  test("testFindStart") {
    findStart(testGrid) shouldBe 5
  }

  test("testFindPath") {
    findPath(testGrid) shouldBe ("ABCDEF", 38)
  }

}

(new Day19Part1Test).execute()
```

This can then be run to get the answer.

```tut:book
import scala.io.Source
val grid = Source.fromResource("day19input.txt").getLines().toVector

findPath(grid)
```

## Part 2

You'll notice above that part one also outputs the path length, which answers
this part. I wasn't quite so prescient to output that for part 1 just because,
but it was such a simple change that I just updated my part one solution to 
output the value, and the test case to check it. 

Puzzle completed.
