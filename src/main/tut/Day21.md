# Fractal Art

A solution for [Advent of Code 2017 - Day 21](http://adventofcode.com/2017/day/21)

## Part 1

Firstly I need to be able to parse the input into something useful, and it would 
be useful to do the rotation/flipping of the input here so that finding the
correct successor image for any pattern is a simple map lookup. Helpfully the
Scala collections library provides a transpose for me, and with som implicit 
magic I can make it fairly clear what is happening.

```tut:book
implicit class MatrixOps[T](m: Vector[Vector[T]]) {
  def flip: Vector[Vector[T]] = m.map(_.reverse)
  def rot: Vector[Vector[T]] = m.transpose.flip
}

def explode(base: Vector[Vector[Char]]): Seq[Vector[Vector[Char]]] = {
  Seq(
    base,
    base.flip,
    base.rot,
    base.rot.flip,
    base.rot.rot,
    base.rot.rot.flip,
    base.rot.rot.rot,
    base.rot.rot.rot.flip
  )
}

def parseInput(lines: TraversableOnce[String]): Map[Vector[Vector[Char]], Vector[Vector[Char]]] = {
  lines.flatMap(
    l => {
      val (base +: result +: _) =
        l.split(" => ")
          .map(r => r.split("/").toVector.map(_.toVector))
          .toSeq

      explode(base).map(_ -> result)
    }
  ).toMap
}
```

With that in place, I can now perform the enhancement rounds. I can use for-
comprehensions to make the looping through the sub-images, doing the pattern
lookup. I also then split the output pattern into single characters, allowing me
to build the new image one bit at a time.

```tut:book
def expand(grid: Vector[Vector[Char]],
           book: Map[Vector[Vector[Char]], Vector[Vector[Char]]],
           rounds: Int): Vector[Vector[Char]] = {

  def _doExpand(i: Int): Vector[Vector[Char]] = {
    val newWidth = (grid(0).length / i) * (i + 1)
    (for {
      x <- 0 until grid(0).length / i
      y <- 0 until grid(0).length / i
      (img, a) <- book(
        (0 until i).map(j =>
          grid((i * x) + j).slice(i * y, (i * y) + i)
        ).toVector
      ).zipWithIndex
      (ch, b) <- img.zipWithIndex
    } yield (x, y, ch, a, b)).foldLeft(Vector.fill(newWidth, newWidth)(' ')) {
      case (acc, (x, y, ch, a, b)) =>
        acc.updated(
          x * (i + 1) + a,
          acc((x * (i + 1)) + a).updated(
            y * (i + 1) + b,
            ch
          )
        )
    }
  }


  if (rounds == 0) grid
  else {
    if (grid(0).length % 2 == 0)
      expand(_doExpand(2), book, rounds - 1)
    else
      expand(_doExpand(3), book, rounds - 1)
  }
}
```

Finally I need to count the pixels in the expanded image to be able to answer
the puzzle.

```tut:book
def countPixels(grid: Vector[Vector[Char]],
                book: Map[Vector[Vector[Char]], Vector[Vector[Char]]],
                rounds: Int): Int = {
  expand(grid, book, rounds).map(_.count(_ == '#')).sum
}
```

There are quite a few steps to this, so also a fairly large set of tests. Mostly
these are derived from the example given.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day21Part1Test extends FunSuite with Matchers {

  test("can malipulate vectors")
  {
    Vector("#.".toVector, "##".toVector).flip shouldBe Vector(".#".toVector, "##".toVector)
    Vector("#.".toVector, "##".toVector).rot shouldBe Vector("##".toVector, "#.".toVector)
    Vector("#.".toVector, "##".toVector).rot.rot shouldBe Vector("##".toVector, ".#".toVector)
    Vector("#.".toVector, "##".toVector).rot.rot.rot shouldBe Vector(".#".toVector, "##".toVector)
    Vector("#.".toVector, "##".toVector).rot.rot.rot.rot shouldBe Vector("#.".toVector, "##".toVector)
  }

  test("can parse input") {
    parseInput(Seq(
      "##/#. => .../.##/##.",
      "##/## => ###/#../#.."
    )) shouldBe Map(
      Vector("##".toVector, "#.".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector(".#".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("#.".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, ".#".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, "##".toVector) -> Vector("###".toVector, "#..".toVector, "#..".toVector)
    )
    parseInput(Seq(
      ".../.../... => .#../.#../#..#/##..",
      "#../.../... => ####/####/.###/####"
    )) shouldBe Map(
      Vector("...".toVector, "...".toVector, "...".toVector) -> Vector(".#..".toVector, ".#..".toVector, "#..#".toVector, "##..".toVector),
      Vector("#..".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("..#".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "#..".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "..#".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector)
    )
    parseInput(Seq(
      ".../.../... => .#../.#../#..#/##..",
      "##./.../... => ####/####/.###/####"
    )) shouldBe Map(
      Vector("...".toVector, "...".toVector, "...".toVector) -> Vector(".#..".toVector, ".#..".toVector, "#..#".toVector, "##..".toVector),
      Vector("##.".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector(".##".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "##.".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, ".##".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("..#".toVector, "..#".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("#..".toVector, "#..".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "#..".toVector, "#..".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "..#".toVector, "..#".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector)
    )

    parseInput(Seq(
      "##/#. => .../.##/##.",
      "##/## => ###/#../#..",
      ".../.../... => .#../.#../#..#/##..",
      "#../.../... => ####/####/.###/####"
    )) shouldBe Map(
      Vector("##".toVector, "#.".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector(".#".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("#.".toVector, "##".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, ".#".toVector) -> Vector("...".toVector, ".##".toVector, "##.".toVector),
      Vector("##".toVector, "##".toVector) -> Vector("###".toVector, "#..".toVector, "#..".toVector),
      Vector("...".toVector, "...".toVector, "...".toVector) -> Vector(".#..".toVector, ".#..".toVector, "#..#".toVector, "##..".toVector),
      Vector("#..".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("..#".toVector, "...".toVector, "...".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "#..".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector),
      Vector("...".toVector, "...".toVector, "..#".toVector) -> Vector("####".toVector, "####".toVector, ".###".toVector, "####".toVector)
    )
  }

  private val book = parseInput(Seq(
    "../.# => ##./#../...",
    ".#./..#/### => #..#/..../..../#..#"
  ))

  private val base = Vector(
    ".#.",
    "..#",
    "###"
  ).map(_.toVector)

  test("can expand image") {
    expand(base, book, 1) shouldBe Vector(
      "#..#",
      "....",
      "....",
      "#..#"
    ).map(_.toVector)

    expand(base, book, 2) shouldBe Vector(
      "##.##.",
      "#..#..",
      "......",
      "##.##.",
      "#..#..",
      "......"
    ).map(_.toVector)
  }


  test("can count pixels image") {
    countPixels(base, book, 2) shouldBe 12
  }
}

(new Day21Part1Test).execute()
```

I can then plug in my puzzle input to get the 'book' of enhancements to apply,
and run the process.

```tut:book
import scala.io.Source
val input = parseInput(Source.fromResource("day21input.txt").getLines())

val base = Vector(
  ".#.",
  "..#",
  "###"
).map(_.toVector)

countPixels(base, input, 5)
```

## Part 2

Part 2 was just part 1 again on a larger scale. I suspect there is some trick to
notice that sections of the image are repeated and that could reduce the problem
to a smaller size. However, pluging the extra enhancement rounds into my
solution above still produced the answer in seconds, so I left it at that.

```tut:book
countPixels(base, input, 18)
```
