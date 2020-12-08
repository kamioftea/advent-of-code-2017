# Electromagnetic Moat

A solution for [Advent of Code 2017 - Day 25](http://adventofcode.com/2017/day/25)

## Part 1

Today's task was to implement a Turing machine. First task was to turn the 
rather verbose input into a usable structure. I used the following case classes 
to represent the various parts.

```tut:book
case class Action(write: Boolean, move: Boolean, next: Char)

case class State(on0: Action, on1: Action)

case class Program(state: Char, iterations: Int, states: Map[Char, State]) {
  lazy val startingState: State = states(state)
}
```

The input itself only really has one useful piece of information on each line,
so I can build a bunch of regular expressions to match each one. They are also
regular enough that I can capture a whole state with a single large pattern
match.

```tut:book
def parseInput(lines: Seq[String]): Program = {
  val StateLine = "Begin in state ([A-Z]).".r
  val state: Char = lines.head match {
    case StateLine(s) => s.charAt(0)
  }
  
  val IterationsLine = "Perform a diagnostic checksum after (\\d+) steps.".r
  val iterations: Int = lines.drop(1).head match {
    case IterationsLine(i) => i.toInt
  }
  
  val StateHeader = "In state ([A-Z]):".r
  val ActionHeader = "  If the current value is (0|1):".r
  val WriteLine = "    - Write the value (0|1).".r
  val MoveLine = "    - Move one slot to the (right|left).".r
  val NextStateLine = "    - Continue with state ([A-Z]).".r
  
  def parseState(s: Seq[String]): (Char, State) = s match {
    case
      StateHeader(l) +:
        ActionHeader("0") +:
        WriteLine(w0) +:
        MoveLine(m0) +:
        NextStateLine(ns0) +:
        ActionHeader("1") +:
        WriteLine(w1) +:
        MoveLine(m1) +:
        NextStateLine(ns1) +: _ =>
      (l.charAt(0), State(
        Action(w0 == "1", m0 == "right", ns0.charAt(0)),
        Action(w1 == "1", m1 == "right", ns1.charAt(0))
      ))
  }
  
  def parseStates(states: Seq[Seq[String]],
                  map: Map[Char, State] = Map.empty): Map[Char, State] = {
    states match {
      case Nil => map
      case s +: ss => parseStates(ss, map + parseState(s))
    }
  }
  
  val states = parseStates(lines.drop(3).grouped(10).toSeq)
  
  Program(state, iterations, states)
}
```

For the actual Turing machine I can just recursively apply each state, using a
`Set[Int]` to record the ribbon positions that are currently set to `1`. Once 
the required number of iterations are complete the size of the set is the number
of `1`s currently set.

```tut:book
def debugChecksum(program: Program): Int = {
  def iter(pos: Int, state: State, ribbon: Set[Int], iterations: Int): Int =
    if (iterations >= program.iterations)
      ribbon.size
    else {
      val action: Action = if (ribbon.contains(pos)) state.on1 else state.on0
      iter(
        pos + (if (action.move) 1 else -1),
        program.states(action.next),
        if (action.write) ribbon + pos else ribbon - pos,
        iterations + 1
      )
    }

  iter(0, program.startingState, Set.empty, 0)
}
```

I can use the example from the puzzle to implement the tests.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day25Test extends FunSuite with Matchers {

  test("can parse input") {
    parseInput("""Begin in state A.
      |Perform a diagnostic checksum after 6 steps.
      |
      |In state A:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state B.
      |  If the current value is 1:
      |    - Write the value 0.
      |    - Move one slot to the left.
      |    - Continue with state B.
      |
      |In state B:
      |  If the current value is 0:
      |    - Write the value 1.
      |    - Move one slot to the left.
      |    - Continue with state A.
      |  If the current value is 1:
      |    - Write the value 1.
      |    - Move one slot to the right.
      |    - Continue with state A.""".stripMargin.lines.toSeq) should be
    Program(
      'A',
      6,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )
  }

  test("can get diagnostic checksum") {
    debugChecksum(Program(
      'A',
      6,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 3

    debugChecksum(Program(
      'A',
      5,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 3

    debugChecksum(Program(
      'A',
      4,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 2

    debugChecksum(Program(
      'A',
      3,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 1

    debugChecksum(Program(
      'A',
      2,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 2

    debugChecksum(Program(
      'A',
      1,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 1

    debugChecksum(Program(
      'A',
      0,
      Map(
        'A' -> State(
          Action(write = true, move = true, 'B'),
          Action(write = false, move = false, 'B')
        ),
        'B' -> State(
          Action(write = true, move = false, 'A'),
          Action(write = true, move = true, 'A')
        )
      )
    )) shouldBe 0
  }

}

(new Day25Test).execute()
```

## Part 2

Part two was just complete all of the previous tasks, so I had nothing more to
do here. 

## Final Stats

```
      --------Part 1--------   --------Part 2--------
Day       Time   Rank  Score       Time   Rank  Score
 25   14:37:45   2672      0   14:38:32   2272      0
 24   05:42:01   1326      0   05:44:54   1268      0
 23   05:44:25   1970      0   07:05:05   1270      0
 22   15:00:25   2927      0   15:15:15   2790      0
 21   17:47:42   2347      0   17:48:18   2274      0
 20   18:20:07   4254      0   18:58:23   3628      0
 19   04:00:04   1743      0   04:02:47   1692      0
 18   10:40:41   3431      0   20:39:06   3475      0
 17   04:08:57   2082      0   04:35:13   1788      0
 16   06:03:26   2613      0   14:47:35   3637      0
 15   03:57:25   2496      0   04:05:26   2298      0
 14   03:51:16   2150      0   04:51:51   1641      0
 13   03:52:54   2574      0   04:08:15   1864      0
 12   07:16:31   3979      0   07:36:47   3768      0
 11   04:00:34   2447      0   04:22:12   2361      0
 10   05:30:16   2690      0   06:18:58   2260      0
  9   09:15:14   4309      0   09:28:09   4221      0
  8   11:57:52   6636      0   12:06:02   6511      0
  7   08:48:52   6247      0   10:47:32   4143      0
  6   06:32:40   5558      0   06:40:59   5303      0
  5   10:28:00   8641      0   10:33:25   8231      0
  4   17:57:50  11957      0   18:15:00  10841      0
  3   04:03:30   2516      0   21:11:27   7616      0
  2   04:41:57   4515      0   16:10:50  10812      0
  1   04:38:15   3757      0   04:46:41   3061      0
```
