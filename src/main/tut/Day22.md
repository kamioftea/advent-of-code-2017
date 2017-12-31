# Sporifica Virus

A solution for [Advent of Code 2017 - Day 22](http://adventofcode.com/2017/day/22)

## Part 1

I put the grid into a map as I can just add extra values as the position expands
outside the current grid, and the Map implementation handles adding the 
underlying data for me.

```tut:book
def parseInput(lines: Seq[String]): Map[Int, Map[Int, Char]] = (
  lines.zipWithIndex
      .map {
        case (line, index) =>
          index - (lines.size / 2) ->
            line.zipWithIndex.map {
              case (c, i) => i - (line.length / 2) -> c
            }.toMap
      }.toMap
)
```

I started with an implementation that had the progress to next stage strategy 
embedded within it, but factored this out into an argument to enable me to use 
the same code for part 2.

```tut:book
def countInfections(grid: Map[Int, Map[Int, Char]],
                    iterations: Int,
                    infectionStrategy: PartialFunction[(Int, Int, Char), (Int, Int, Char)]): Int = {
  def iter(grid: Map[Int, Map[Int, Char]],
           x: Int, y: Int,
           dx:Int, dy: Int,
           count: Int, infectionCount: Int): Int = {
    if(count == iterations) return infectionCount

    val status = grid.getOrElse(y, Map.empty).getOrElse(x, '.')
    val (ndx, ndy, nStatus) = infectionStrategy((dx, dy, status))
    val nICount = if(nStatus == '#') infectionCount + 1 else infectionCount
    iter(
      grid.updated(y, grid.getOrElse(y, Map.empty).updated(x, nStatus)),
      x + ndx, y + ndy,
      ndx, ndy,
      count + 1, nICount
    )
  }

  iter(grid, 0, 0, 0, -1, 0, 0)
}

val basicStrategy: PartialFunction[(Int, Int, Char), (Int, Int, Char)] = {
  case (dx, dy, '.') => (dy, -dx, '#')
  case (dx, dy, '#') => (-dy, dx, '.')
}
```

These can now be tested.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day22Part1Test extends FunSuite with Matchers {
  test("can parse input") {
    parseInput(
      """..#
        |#..
        |...""".stripMargin.lines.toSeq
    ) shouldBe Map(
      -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
      0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
      1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
    )
  }

  test("can count infections") {
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      7,
      basicStrategy
    ) shouldBe 5
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      70,
      basicStrategy
    ) shouldBe 41
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      10000,
      basicStrategy
    ) shouldBe 5587
  }
}

(new Day22Part1Test).execute()
```

Now I can let the virus loose on the puzzle input

```tut:book
import scala.io.Source
val input = parseInput(Source.fromResource("day22input.txt").getLines().toSeq)

countInfections(input, 10000, basicStrategy)
```

## Part 2

As I briefly mentioned above, my code from part one was mostly reusable for part
2. I factored out the step that produced the character to write at the current
position, and the new direction. Once this was done and working with the 
original tests, I then wrote the updated function to handle the evolved virus,
and tested it against the puzzle example.

```tut:book
val evolvedStrategy: PartialFunction[(Int, Int, Char), (Int, Int, Char)] = {
  case (dx, dy, '.') => (dy, -dx, 'W')
  case (dx, dy, 'W') => (dx, dy, '#')
  case (dx, dy, '#') => (-dy, dx, 'F')
  case (dx, dy, 'F') => (-dx, -dy, '.')
}

class Day22Part2Test extends FunSuite with Matchers {
  test("can evolve"){
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      100,
      evolvedStrategy
    ) shouldBe 26
    countInfections(
      Map(
        -1 -> Map(-1 -> '.', 0 -> '.', 1 -> '#'),
        0 -> Map(-1 -> '#', 0 -> '.', 1 -> '.'),
        1 -> Map(-1 -> '.', 0 -> '.', 1 -> '.')
      ),
      10000000,
      evolvedStrategy
    ) shouldBe 2511944
  }
}

(new Day22Part2Test).execute()
```

My solution handled the increased iteration count fine, running both parts in
about 5 seconds.

```tut:book
countInfections(input, 10000000, evolvedStrategy)
```
