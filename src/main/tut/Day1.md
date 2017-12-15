# Inverse Captcha

A solution for [Advent of Code 2017 - Day 1](http://adventofcode.com/2017/day/1)

... And we're off. I shall be briefly documenting my solutions to each of the
puzzled in the 2017 Advent of Code. Whilst I'll be completing the puzzles on the
day I'm planning to schedule the write-ups to appear one week later. 

## Part 1 

The aim is to fool a computer that I'm a computer, so I shall be talking to it
with code. The challenge is to sum repeated digits in a long sequence of them. 
It is a challenge particularly suited to composing the built in functions of the 
Scala collections library to produce a simple(ish), solution that reads like a
recipe. The only slight spanner in the works is the list is cyclical so the 
final digit needs to be checked against the first. 

```tut:book
def sumDuplicates(digits: Seq[Int]): Int = (
  digits
    .zip(digits.tail :+ digits.head)
    .collect { case (a, b) if a == b => a }
    .sum
  )
``` 
<small>
  I'm intending to do a write up of the process used to generate the code
  samples in these write-ups, but the plugin I'm using doesn't work out
  statement termination as well as the full scala compiler. Hence you may
  see occasional superfluous parentheses as compiler hints.
</small>

Now I should check my working; especially since the puzzle kindly included some 
test cases.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day1Part1Test extends FunSuite with Matchers {
  test("can Sum Duplicates") {
    sumDuplicates("1122".map(_.asDigit)) shouldBe 3
    sumDuplicates("1111".map(_.asDigit)) shouldBe 4
    sumDuplicates("1234".map(_.asDigit)) shouldBe 0
    sumDuplicates("91212129".map(_.asDigit)) shouldBe 9
    sumDuplicates("91222129".map(_.asDigit)) shouldBe 13
  }
}

(new Day1Part1Test).execute()
```

Passing in my puzzle input, and the !captcha is partially fooled:

```tut:book
import scala.io.Source

val input: Seq[Int] = Source.fromResource("day1input.txt").mkString("").trim.map(_.asDigit)
      
sumDuplicates(input)
```

## Part 2

The second part is normally some form of extension of the first. In this case 
I now need to compare each digit to its mirror; the digit half-way round the 
circular list. I can pretty much reuse my initial solution, but change how the 
pairs are built. Additionally for each pair, the inverse pair will also match.

```tut:book
def sumMirrors(digits: Seq[Int]): Int = {
  val (first, second) =
    digits.splitAt(digits.length / 2)
  
  (first
    .zip(second)
    .collect { case (a, b) if a == b => a * 2 }
    .sum
  )
}
```

As before there are some test cases provided.

```tut:book
class Day1Part2Test extends FunSuite with Matchers {
  test("can Sum Mirrors") {
      sumMirrors("1212".map(_.asDigit)) shouldBe 6
      sumMirrors("1221".map(_.asDigit)) shouldBe 0
      sumMirrors("123425".map(_.asDigit)) shouldBe 4
      sumMirrors("123123".map(_.asDigit)) shouldBe 12
      sumMirrors("12131415".map(_.asDigit)) shouldBe 4
    }
}

(new Day1Part2Test).execute()
```

And the puzzle input is reused

```tut:book
sumMirrors(input)
```
