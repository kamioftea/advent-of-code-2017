# Corruption Checksum

A solution for [Advent of Code 2017 - Day 2](http://adventofcode.com/2017/day/2)

## Part 1 

I'm given a tab separated spreadsheet and need to calculate a checksum by 
finding the difference between the minimum and maximum of each line, and then 
summing those values.

First I'll make the input into something usable:

```tut:book
val part1input = """5 1 9 5
                   |7 5 3
                   |2 4 6 8""".stripMargin
                 
def parseRows(sheet: Seq[String]): Seq[Seq[Int]] =
    sheet.map(
      l => l.split("\\s").map(s => s.toInt).toSeq
    )
    
val part1sheet: Seq[Seq[Int]] = 
  parseRows(part1input.lines.toSeq)
```

Now I can try to calculate a checksum...

```tut:book
def calcRowChecksum(row: Seq[Int]): Int = {
    val (rowMin, rowMax) = row.foldLeft((Int.MaxValue, Int.MinValue)){
      case ((min, max), cell) => (Math.min(min, cell), Math.max(max, cell))
    }
    
    rowMax - rowMin
}

def calcSheetChecksum(sheet: Seq[Seq[Int]]): Int = 
  sheet.map(calcRowChecksum).sum
```

...and check my working using the examples in the puzzle:

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day2Part1Test extends FunSuite with Matchers {
  
  test("Can calculate per row checksums") {
    calcRowChecksum(part1sheet(0)) shouldBe 8
    calcRowChecksum(part1sheet(1)) shouldBe 4
    calcRowChecksum(part1sheet(2)) shouldBe 6
  }
  
  test("Can calculate whole sheet checksums") { 
    calcSheetChecksum(part1sheet) shouldBe 18
  }
}

(new Day2Part1Test).execute()
```

Finally I can pass in the puzzle input and get an answer.

```tut:book
import scala.io.Source

val puzzleSheet = parseRows(
  Source.fromResource("day2input.txt").getLines().toSeq
)

calcSheetChecksum(puzzleSheet)
```

## Part 2

It turns out what I actually need to do is find the two evenly divisible 
numbers on each row, and sum the common factor.

```tut:book

def findDivisor(needle: Int, haystack: Seq[Int]): Option[Int] = haystack match {
  case Nil => None
  case h +: _ if (needle % h) == 0 => Some(needle / h)
  case h +: _ if (h % needle) == 0 => Some(h / needle)
  case _ +: rest => findDivisor(needle, rest)
}

def findFactor(row: Seq[Int]): Int = row match {
  case Nil => 0 // Should never occur with out input
  case h +: t => 
    val maybeDivisor = findDivisor(h, t)
    
    // Would normally use getOrElse but the compiler can't detect that as
    // tail recursive
    if(maybeDivisor.isDefined) maybeDivisor.get
    else findFactor(t)
}

def sumFactors(sheet: Seq[Seq[Int]]) = sheet.map(findFactor).sum
```

There is different test input for this, and some more examples:

```tut:book
val part2input = """5 9 2 8
                   |9 4 7 3
                   |3 8 6 5""".stripMargin
    
val part2sheet: Seq[Seq[Int]] = 
  parseRows(part2input.lines.toSeq)

class Day2Part2Test extends FunSuite with Matchers {
  
  test("Can calculate per row factors") {
    findFactor(part2sheet(0)) shouldBe 4
    findFactor(part2sheet(1)) shouldBe 3
    findFactor(part2sheet(2)) shouldBe 2
  }
  
  test("Can calculate the sum of the sheet's factors") { 
    sumFactors(part2sheet) shouldBe 9
  }
}

(new Day2Part2Test).execute()
```

Putting it all together

```tut:book
sumFactors(puzzleSheet)
```

