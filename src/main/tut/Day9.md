# Stream Processing

A solution for [Advent of Code 2017 - Day 9](http://adventofcode.com/2017/day/9)

## Part 1

Today's task is kind of a twist on ensuring your curly braces match up. Not really much of a problem now that compilers and IDEs can do that for you. The twist is that as well as the curly braces there are angle braces, and these change the rules for how characters should be parsed when encountered.

This puzzle is crying out for a state machine. I used a bunch of case classes, probably more than was needed for the puzzle given that the provided stream doesn't need validating against the syntax. For the puzzle I only need to enough interpretation to keep track of nesting depth, parsing mode (garbage/not), and when a group closes. I decided to instead essintially emit a state between each character. This then had a function to get the next state given the following character. Whilst a bit more verbose, it makes it easier to read what is going on, and is more likely to be reusable for part 2.

```tut:book
sealed trait State {
  def apply(char: Char): State
}

case object ExpectOpen extends State {
  override def apply(char: Char): State = char match {
    case '{' => StartGroup
    case '<' => StartGarbage
    case _ => Error("ExpectOpen: expected '{' or '<'; got " + char)
  }
}

case object StartGroup extends State {
  override def apply(char: Char): State = char match {
    case '{' => StartGroup
    case '<' => StartGarbage
    case '}' => CloseGroup
    case _ => Error("ExpectOpen: expected '{', '}', or '<'; got " + char)
  }
}

case object CloseGroup extends State {
  override def apply(char: Char): State = char match {
    case ',' => ExpectOpen
    case '}' => CloseGroup
    case _ => Error("StartGroup: expected '}' or ','; got " + char)
  }
}

case object InGarbage extends State {
  override def apply(char: Char): State = char match {
    case '>' => CloseGarbage
    case '!' => Escaped
    case _ => InGarbage
  }
}

// In initially for part 1 this didn't exist and there was just InGarbage.
// It is easier to include it here than have to redefine all the states again 
// for part 2
case object StartGarbage extends InGarbage

case object Escaped extends State {
  override def apply(char: Char): State = StartGarbage
}

case object CloseGarbage extends State {
  override def apply(char: Char): State = char match {
    case ',' => ExpectOpen
    case '}' => CloseGroup
    case _ => Error("StartGroup: expected '}' or ','; got " + char)
  }
}
```

To map the stream of chars from the input to a stream of states I can pull a nifty trick of lazily defining a Stream starting with the initial state. Then to get the rest of that stream I append the Stream[Chars] zipped with the stream itself. Since the first value is defined, that gets zipped with the first character, which is enough to compute the second state. Thanks to the lazy evaluation, that second state is then available to be zipped with the second Char, producing a third state, and so on...

```tut:book
def parseStream(chars: Stream[Char]): Stream[State] = {

  lazy val states: Stream[State] =
    ExpectOpen #:: states.zip(chars).map { case (s, c) => s(c) }

  states
}

```

Now there is a stream of states it is possible to filter to just the ones needed to calculate the current depth (Start Group increments, End Group decrements) and a running total (one of Start or End Group, I chose End as it means the current value of depth is the correct depth to add to the score.)

```tut:book
def scoreGroups(states: Stream[State]): Int = {
  @tailrec
  def iter(states: Stream[State], depth: Int = 0, score: Int = 0): Int =
    states match {
      case Stream.Empty => score
      case StartGroup #:: tail => iter(tail, depth + 1, score)
      case CloseGroup #:: tail => iter(tail, depth - 1, score + depth)
      case _ #:: tail => iter(tail, depth, score)
    }

  iter(states)
}
```

Both the parsing and the reduction need some testing. Today there is a wealth of test cases from the puzzle description. Even abstracting out some repeated patterns the stream
parsing tests are quite verbose, but as I'm essentially mapping each character to multiple words that is expected.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day9Part1Test extends FunSuite with Matchers {

  test("can parse stream") {

    def repeat[T](n: Int, value: T): Stream[T] = {
      if (n == 0) Stream.empty
      else value #:: repeat(n - 1, value)
    }

    parseStream("<>,".toStream) shouldBe Stream(ExpectOpen, StartGarbage, CloseGarbage, ExpectOpen)

    parseStream("<random characters>,".toStream) shouldBe Stream(
      Stream(ExpectOpen, StartGarbage),
      repeat(17, InGarbage),
      Stream(CloseGarbage, ExpectOpen)
    ).flatten

    parseStream("<<<<>,".toStream) shouldBe Stream(
      Stream(ExpectOpen, StartGarbage),
      repeat(3, InGarbage),
      Stream(CloseGarbage, ExpectOpen)
    ).flatten

    parseStream("<{!>}>,".toStream) shouldBe Stream(
      ExpectOpen,
      StartGarbage,
      InGarbage,
      Escaped,
      StartGarbage,
      InGarbage,
      CloseGarbage,
      ExpectOpen
    )

    parseStream("<!!>,".toStream) shouldBe Stream(
      ExpectOpen,
      StartGarbage,
      Escaped,
      StartGarbage,
      CloseGarbage,
      ExpectOpen
    )

    parseStream("<!!!>>,".toStream) shouldBe Stream(
      ExpectOpen,
      StartGarbage,
      Escaped,
      StartGarbage,
      Escaped,
      StartGarbage,
      CloseGarbage,
      ExpectOpen
    )

    parseStream("<{o\"i!a,<{i<a>,".toStream) shouldBe Stream(
      Stream(ExpectOpen, StartGarbage),
      repeat(4, InGarbage),
      Stream(Escaped, StartGarbage),
      repeat(6, InGarbage),
      Stream(CloseGarbage, ExpectOpen)
    ).flatten

    parseStream("{},".toStream) shouldBe Stream(
      ExpectOpen,
      StartGroup,
      CloseGroup,
      ExpectOpen
    )

    parseStream("{},".toStream).count(_ == StartGroup) shouldBe 1

    parseStream("{{{}}},".toStream) shouldBe Stream(
      Stream(ExpectOpen),
      repeat(3, StartGroup),
      repeat(3, CloseGroup),
      Stream(ExpectOpen)
    ).flatten

    parseStream("{{{}}},".toStream).count(_ == StartGroup) shouldBe 3

    parseStream("{{},{}},".toStream) shouldBe Stream(
      Stream(ExpectOpen),
      repeat(2, StartGroup),
      Stream(CloseGroup, ExpectOpen, StartGroup),
      repeat(2, CloseGroup),
      Stream(ExpectOpen)
    ).flatten

    parseStream("{{{}}},".toStream).count(_ == StartGroup) shouldBe 3

    parseStream("{{{},{},{{}}}},".toStream) shouldBe Stream(
      Stream(ExpectOpen),
      repeat(2, StartGroup),
      repeat(2, Stream(StartGroup, CloseGroup, ExpectOpen)).flatten,
      repeat(2, StartGroup),
      repeat(4, CloseGroup),
      Stream(ExpectOpen)
    ).flatten

    parseStream("{{{},{},{{}}}},".toStream).count(_ == StartGroup) shouldBe 6

    parseStream("{<a>,<a>,<a>,<a>},".toStream).count(_ == StartGroup) shouldBe 1
    parseStream("{{<a>},{<a>},{<a>},{<a>}},".toStream).count(_ == StartGroup) shouldBe 5
    parseStream("{{<!>},{<!>},{<!>},{<a>}},".toStream).count(_ == StartGroup) shouldBe 2

  }

  test("can claculate the group score") {

    scoreGroups(parseStream("{},".toStream)) shouldBe 1
    scoreGroups(parseStream("{{{}}},".toStream)) shouldBe 6
    scoreGroups(parseStream("{{},{}},".toStream)) shouldBe 5
    scoreGroups(parseStream("{{{},{},{{}}}},".toStream)) shouldBe 16
    scoreGroups(parseStream("{<a>,<a>,<a>,<a>},".toStream)) shouldBe 1
    scoreGroups(parseStream("{{<ab>},{<ab>},{<ab>},{<ab>}},".toStream)) shouldBe 9
    scoreGroups(parseStream("{{<!!>},{<!!>},{<!!>},{<!!>}},".toStream)) shouldBe 9
    scoreGroups(parseStream("{{<a!>},{<a!>},{<a!>},{<ab>}},".toStream)) shouldBe 3

  }
}

(new art1Test).execute()
```

With those passing, I can get the score for the full puzzle input.

```tut:book
import scala.io.Source
def input = Source.fromResource("day9input.txt").toStream

scoreGroups(parseStream(input)
```

## Part 2

For this I needed to analyse the garbage instead of the groups. You may have noted the comment about StartGarbage not existing when I wrote part 1. This was becuase there was no need to distiguish if we'd just read the opening < of a garbage sequene, and the rest of the sequence. Part two however asked for just that. So I refactored the states to distingish start from in, and the rest was just counting.

```tut:main
def countGarbageChars(states: Stream[State]): Int =
  states.count(_ == InGarbage)

```

The parsing tests still work, so the refactor had been completed correctly. Reworking the tests for part 2 gives the following.

```tut:book
class Day9Part2Test extends FunSuite with Matchers {
  test("canCountGarbage") {
    countGarbageChars(parseStream("<>,".toStream)) shouldBe 0
    countGarbageChars(parseStream("<random characters>,".toStream)) shouldBe 17
    countGarbageChars(parseStream("<<<<>,".toStream)) shouldBe 3
    countGarbageChars(parseStream("<{!>}>,".toStream)) shouldBe 2
    countGarbageChars(parseStream("<!!>,".toStream)) shouldBe 0
    countGarbageChars(parseStream("<!!!>>,".toStream)) shouldBe 0
    countGarbageChars(parseStream("<{o\"i!a,<{i<a>,".toStream)) shouldBe 10
    countGarbageChars(parseStream("{{<!!>},{<!!>},{<!!>},{<!!>}},".toStream)) shouldBe 0
    countGarbageChars(parseStream("{{<a!>},{<a!>},{<a!>},{<ab>}},".toStream)) shouldBe 17
  }
}

(new Day9Part2Test).execute()
```

This can then be applied to the puzzle input

```tut:book
countGarbageChars(parseStream(input)
```
