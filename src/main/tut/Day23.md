# Coprocessor Conflagration

A solution for [Advent of Code 2017 - Day 23](http://adventofcode.com/2017/day/23)

## Part 1

The first part of today's task is a reworking of 
[day 18 part 1](/aoc-2017-day-18-duet). As I mentioned in my write up of that, 
there were some lessons I learnt from solving part 2 that I wanted to work back
into my parser. 

Firstly there was a bug where by I had been treating the first column of values 
as always being references, and this didn't hold. Splitting out the value type
in the regex as before was very hard to read, so I factored that out into a
function that was called for each match that could be either type. Secondly 
passing in the current registers when trying to extract a value from an
instruction caused a lot of noise and it was hard to read, so I used implicits
to hide the boiler-plate.

```tut:book
sealed trait Value {
  def read(implicit registers: Map[Char, Long]): Long
}

case class Ref(register: Char) extends Value {
  override def read(implicit registers: Map[Char, Long]): Long =
    registers.getOrElse(register, 0)
}

case class Literal(value: Long) extends Value {
  override def read(implicit registers: Map[Char, Long]): Long = value
}

case class Instruction(command: String, value1: Value, value2: Value)

implicit def valueFromString(str: String): Value = (
  if (str.matches("[a-z]")) Ref(str.charAt(0))
  else Literal(str.toLong)
)

val LineMatcher = "(set|sub|mul|jnz) ([a-z]|-?\\d+) ([a-z]|-?\\d+)".r

def instFromLine(line: String): Instruction = line match {
  case LineMatcher(c, v1, v2) => Instruction(c, v1, v2)
}

def countMults(program: Vector[Instruction], regs: Map[Char, Long] = Map.empty): Int = {
  def iter(registers: Map[Char, Long], position: Int, mulCount: Int): Int = {
    if (!program.isDefinedAt(position)) mulCount
    else {
      implicit val _ = registers
      program(position) match {
        case Instruction("set", Ref(r), v2) =>
          iter(registers.updated(r, v2.read), position + 1, mulCount)
        case Instruction("sub", Ref(r), v2) =>
          iter(registers.updated(r, registers(r) - v2.read), position + 1, mulCount)
        case Instruction("mul", Ref(r), v2) =>
          iter(registers.updated(r, registers(r) * v2.read), position + 1, mulCount + 1)
        case Instruction("jnz", v1, v2) =>
          iter(registers, if (v1.read != 0) position + v2.read.toInt else position + 1, mulCount)
      }
    }
  }

  iter(regs.withDefaultValue(0), 0, 0)
}
```

I created an example instruction for each of the possible instruction types and
made sure to include the static jnz test that was missing last time and caught 
me out. I also built a small test program to check that the interpreter was 
correct.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day23Part1Test extends FunSuite with Matchers {

  test("testInstFromLine") {
    instFromLine("sub a 23") shouldBe Instruction("sub", Ref('a'), Literal(23))
    instFromLine("mul b -2") shouldBe Instruction("mul", Ref('b'), Literal(-2))
    instFromLine("set b 4") shouldBe Instruction("set", Ref('b'), Literal(4))
    instFromLine("jnz c d") shouldBe Instruction("jnz", Ref('c'), Ref('d'))
    instFromLine("jnz 1 2") shouldBe Instruction("jnz", Literal(1), Literal(2))
  }

  test("testCountMults") {
    countMults(
      """set a 4
        |set b 1
        |mul b a
        |jnz a 2
        |jnz 1 3
        |sub a 1
        |jnz 1 -4""".stripMargin.lines.map(instFromLine).toVector
    ) shouldBe 5

    countMults(
      """set a 8
        |set b 1
        |mul b a
        |jnz a 2
        |jnz 1 3
        |sub a 1
        |jnz 1 -4""".stripMargin.lines.map(instFromLine).toVector
    ) shouldBe 9
  }

}

(new Day23Part1Test).execute()
``` 

With this in place I can get the `mul` instruction count when running the code.

```tut:book
import scala.io.Source

val program = (
  Source.fromResource("day23input.txt")
    .getLines()
    .map(instFromLine)
    .toVector
)

countMults(program)
```

## Part 2

For part two I initially tried running the code with the debug flag set
`countMults(program, Map('a' -> 1))`, but this was obviously not going to 
complete anytime soon. I put in a few debug `println`s to try and get a handle
on what was going on and ended up annotating and optimising the program in place

The first section is just initialisation, and is how the debug flag actually
influences the execution. 
```
  set b 79       | Intiialise b and c, this is where diferent  
  set c b        | coder's inputs were differentiated. In my 
  jnz a 2        | case, 79.
  jnz 1 5        | Then if a != 0 multiply by 100, add 100,000,
  mul b 100      | and add 17000 to c. Which is how the scale
  sub b -100000  | problems I encountered were introduced:
  set c b        | - a=0: Two nested loops go from 2 to 79
  sub c -17000   | - a=1: 1,000 x above, loops are 2 to 107,900+ 
```

From there there are a number of loops. I've labeled the jnz targets below on
the left. Then it is noticeable that g is often used in a pattern:
* Set g = var1
* Subtract var2 from g
* If g != 0 skip the next line/loop

Which is equivalent to if(var1 == var2) set a thing or break out of the current 
loop. I've broken the program down to the loop structure, and factored out the 
lines using g into the equivalent if statements.  
```
G: set f 1       | :outer loop
   set d 2       |   set f = 1, d = 2
C: set e 2       |   :middle loop {
                 |      set e = 2
B: set g d       |      :inner loop {
   mul g e       |        
   sub g b       |        
>A jnz g 2       |        if (d * e) == b set f = 0
   set f 0       |        
A: sub e -1      |        
   set g e       |        
   sub g b       |        if(e++ == b) break  
>B jnz g -8      |      } 
   sub d -1      |      
   set g d       |      
   sub g b       |      if(d++ == b) break
>C jnz g -13     |   } 
>D jnz f 2       |   
   sub h -1      |   if (f == 0) h++
D: set g b       |   
   sub g c       |   
>E jnz g 2       |   if(b == c) exit  
>? jnz 1 3       |   
E: sub b -17     |   b += 17
>G jnz 1 -23     | }

The inner loop and middle loops are essentially looping `d` and `e` through all 
the integers between 2 and the current `b` setting a flag if `d * e == b`, 
but still completing both loops. The inner loop can be refactored into 
`if (b % d == 0) f = true`. Once this is reached we can also break out of both 
loops. This means I can write the middle loop in Scala as

```tut:book
def hasFactor(b: Int): Boolean = {
  def iter(d: Int): Boolean = {
    if(d == b) return false
    if(b % d == 0) return true
    iter(d + 1)
  }
  
  iter(2)
}
```

The outer loop is then incrementing b by 17 each time and incrementing a counter
if b has a factor. Incorporating the initialisation from the initial value of `b`
which then makes this a general implementation for any of the individual inputs
as far as I can tell:

```tut:book
def decomposed(seed: Int): Int =
{
  val start = seed * 100 + 100000
  val end = start + 17000

  def iter(b: Int, h: Int = 0): Int = {
    if(b > end) return h
    iter(b + 17, if(hasFactor(b)) h + 1 else h)
  }

  iter(start)
}

decomposed(79)
```

It is worth noting that whilst there is nothing obvious to be done about the 
inefficient modulus  with the instruction set available, the early break can be 
implemented. Given that 907 of the 1000 outer loops would then break after less 
than &#8730;124,900 iterations of the middle loop. This probably isn't enough to
make it viable, but still a major improvement. The change would be fairly simple:
add `jnz 1 9` after `set f = 0`, and increment the `jnz` instructions for the 
loops outside that by 1 to account for the extra instruction.
