# Electromagnetic Moat

A solution for [Advent of Code 2017 - Day 24](http://adventofcode.com/2017/day/24)

## Part 1

For this task I have to build a bridge by combining pairs of integers like 
dominoes. Firstly I need the data in a useful representation. The components
I represent as a case class, and include a unique id so that if there are any 
duplicated components, they still count as different when checking if they 
have been used before. I then build a map that can look up the components by
either connector.

```tut:book
case class Component(id: Int, a: Int, b: Int)

def parseInput(lines: Seq[String]): Map[Int, Set[Component]] = {
  lines
  .map(_.split("/").toSeq)
  .zipWithIndex.collect {
    case ((a +: b +: _), id) => Component(id, a.toInt, b.toInt)
  }
  .foldLeft(Map.empty[Int, Set[Component]]) {
    case (acc, comp) =>
      acc
      .updated(comp.a, acc.getOrElse(comp.a, Set.empty) + comp)
      .updated(comp.b, acc.getOrElse(comp.b, Set.empty) + comp)
  }
}
```

To get the strongest bridge I'm just going to create all the possible bridges
and find the maximum strength. This is wildly inefficient, but I can optimise
it later if it turns out not to run quickly enough. 

Building the bridges can be done recursively. I only need to know what is in the 
bridge so far and how many connectors the current end piece has. From that, my 
pool of components is already indexed by connector count, so I can grab the 
viable pieces and iterate for each of the new, longer bridges.

```tut:book
def buildBridges(components: Map[Int, Set[Component]]): Seq[Seq[Component]] = {
  def iter(bridge: Seq[Component], openPort: Int): Seq[Seq[Component]] = {
    bridge +:
      components.getOrElse(openPort, Seq.empty)
      .filter(!bridge.contains(_))
      .flatMap(c => iter(c +: bridge, if (c.a == openPort) c.b else c.a)).toSeq
  }

  iter(Seq.empty, 0)
}

def scoreBridge(bridge: Seq[Component]): Int =
  bridge.map(b => b.a + b.b).sum

def strongestBridge(components: Map[Int, Set[Component]]): Int = {
  buildBridges(components)
  .map(scoreBridge)
  .max
  }
```

I can now parse the sample data, check that it is represented correctly, and 
that the output of the various stages matches the examples from the puzzle.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day24Part1Test extends FunSuite with Matchers {
  test("can parse input") {
    parseInput(
      """0/2
        |2/2
        |2/3
        |3/4
        |3/5
        |0/1
        |10/1
        |9/10""".stripMargin.lines.toSeq
    ) shouldBe Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    )
  }
  
  test("can build bridges") {
    val bridges = buildBridges(Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    ))

    bridges should contain(Seq(Component(5,0,1)))
    bridges should contain(Seq(Component(6, 10, 1), Component(5,0,1)))
    bridges should contain(Seq(Component(7, 9, 10), Component(6, 10, 1), Component(5, 0, 1)))
    bridges should contain(Seq(Component(0, 0, 2)))
    bridges should contain(Seq(Component(2, 2, 3), Component(0, 0, 2)))

    bridges should contain(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse)
    bridges should contain(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse)
  }

  test("can score bridges") {
    scoreBridge(Seq(Component(5,0,1))) shouldBe 1
    scoreBridge(Seq(Component(6, 10, 1), Component(5,0,1))) shouldBe 12
    scoreBridge(Seq(Component(7, 9, 10), Component(6, 10, 1), Component(5, 0, 1))) shouldBe 31
    scoreBridge(Seq(Component(0, 0, 2))) shouldBe 2
    scoreBridge(Seq(Component(2, 2, 3), Component(0, 0, 2))) shouldBe 7
    scoreBridge(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse) shouldBe 14
    scoreBridge(Seq(Component(0, 0, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse) shouldBe 15
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2)).reverse) shouldBe 6
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3)).reverse) shouldBe 11
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(3, 3, 4)).reverse) shouldBe 18
    scoreBridge(Seq(Component(0, 0, 2), Component(1, 2, 2), Component(2, 2, 3), Component(4, 3, 5)).reverse) shouldBe 19
  }

  test("can find strongest bridge") {
    strongestBridge(Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    )) shouldBe 31
  }
}

(new Day24Part1Test).execute()
```

I can then parse the test input and compute a result.

```tut:book
import scala.io.Source
val components = parseInput(Source.fromResource("day24input.txt").getLines().toSeq)
strongestBridge(components)
```

## Part 2

Since I'm already building all the bridges, I can re-run part one, but with a
different comparator. As ties are broken by strongest I can take advantage of
scala already having and ordering defined for tuples of order by the first 
element, then second, etc.

```tut:book
def longestBridge(components: Map[Int, Set[Component]]): Int = {
  buildBridges(components)
  .map(b => (b.length, scoreBridge(b)))
  .max
  ._2
}

class Day24Part2Test extends FunSuite with Matchers {
  test("can find longest bridge") {
    longestBridge(Map(
      0 -> Set(Component(0, 0, 2), Component(5, 0, 1)),
      1 -> Set(Component(6, 10, 1), Component(5, 0, 1)),
      2 -> Set(Component(0, 0, 2), Component(1, 2, 2), Component(2,2,3)),
      3 -> Set(Component(2, 2, 3), Component(3,3,4),Component(4,3,5)),
      4 -> Set(Component(3,3,4)),
      5 -> Set(Component(4,3,5)),
      9 -> Set(Component(7, 9,10)),
      10 -> Set(Component(7, 9,10), Component(6, 10, 1))
    )) shouldBe 19
  }
}

(new Day24Part2Test).execute()

longestBridge(components)
```
