# Particle Swarm

A solution for [Advent of Code 2017 - Day 20](http://adventofcode.com/2017/day/20)

## Part 1

For starters with today's input I'm going to need to turn my puzzle input into
something useful.

```tut:book
case class Coordinate(x: Long, y: Long, z: Long) {
  def +(that: Coordinate): Coordinate = copy(x = x + that.x, y = y + that.y, z = z + that.z)
  lazy val mag: Long = Math.abs(x) + Math.abs(y) + Math.abs(z)
}

case class Particle(index: Long, pos: Coordinate, vel: Coordinate, acc: Coordinate) {
  def next: Particle = copy(pos = pos + vel + acc, vel = vel + acc)
}

val LineMatcher = "p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>".r

def parseLines(lines: TraversableOnce[String]): Seq[Particle] =
  lines.toSeq.zipWithIndex.collect {
    case (LineMatcher(px, py, pz, vx, vy, vz, ax, ay, az), i) => Particle(
      i,
      Coordinate(px.toLong, py.toLong, pz.toLong),
      Coordinate(vx.toLong, vy.toLong, vz.toLong),
      Coordinate(ax.toLong, ay.toLong, az.toLong)
    )
  }
```

Given that in the limit, the closest particle is going to be among those with 
the least acceleration, my first idea is to sort the particles by that and see
what the slowest look like.

```tut:book
import scala.io.Source
val input = parseLines(Source.fromResource("day20input.txt").getLines())

implicit val coordinateOrder: Ordering[Coordinate] = Ordering.by(_.mag)
input.sortBy(_.acc).take(3).foreach(println)
```

Which makes things easy as I can now just submit particle 300 as the answer.

First I'll check my working:

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day20Part1Test extends FunSuite with Matchers {
  test("can parse lines") {
    parseLines(
      """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
        |p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>""".stripMargin.lines
    ) shouldBe Seq(
      Particle(
        0,
        Coordinate(3, 0, 0),
        Coordinate(2, 0, 0),
        Coordinate(-1, 0, 0)
      ),
      Particle(
        1,
        Coordinate(4, 0, 0),
        Coordinate(0, 0, 0),
        Coordinate(-2, 0, 0)
      )
    )
  }

  test("coordinates can be ordered") {
    Seq(
      Coordinate(0, -1, 0),
      Coordinate(0, -1, 3),
      Coordinate(2, 0, 0),
      Coordinate(0, 0, 0),
      Coordinate(1, -1, 1)
    ).sorted shouldBe Seq(
      Coordinate(0, 0, 0),
      Coordinate(0, -1, 0),
      Coordinate(2, 0, 0),
      Coordinate(1, -1, 1),
      Coordinate(0, -1, 3)
    )
  }
}
(new Day20Part1Test).execute()
```

## Part 2

For part two I think the best first step is just to run the simulation until the
particle count is roughly stable. Technically I'll need to run it until all the 
particles are moving away from each other, but working that out will not be easy
and probably unnecessary.

Determining the collisions is just a case of finding particles that share the 
same position as another, and filtering them out. I picked 1000 iterations of 
the particle count staying the same as a reasonable approximation of determining
when the collisions were complete.

```tut:book
def countSurvivingParticles(particles: Seq[Particle], count: Long = 0, prevLength: Int = 0): Int = {
  val newParticles: Seq[Particle] =
    particles
      .groupBy(_.pos)
      .filter {case (_, ps) => ps.lengthCompare(1) == 0}
      .flatMap(_._2)
      .map(_.next)
      .toSeq
      
  if(count % 1000 == 0) {
    if(newParticles.lengthCompare(prevLength) == 0)
      newParticles.length
    else
      countSurvivingParticles(newParticles, count + 1, newParticles.length)
  }
  else countSurvivingParticles(newParticles, count + 1, prevLength)
} 
```
 
I can use the example from the puzzle to provide a test case. I also still need
to write tests for advancing particles to the next state.
 
```tut:book
class Day20Part2Test extends FunSuite with Matchers {
  test("can step particles") {
    Particle(
      0,
      Coordinate(3, 0, 0),
      Coordinate(2, 0, 0),
      Coordinate(-1, 0, 0)
    ).next shouldBe
      Particle(
        0,
        Coordinate(4, 0, 0),
        Coordinate(1, 0, 0),
        Coordinate(-1, 0, 0)
      )
  
    Particle(
      0,
      Coordinate(3, 0, 0),
      Coordinate(2, 0, 0),
      Coordinate(-1, 0, 0)
    ).next.next shouldBe
      Particle(
        0,
        Coordinate(4, 0, 0),
        Coordinate(0, 0, 0),
        Coordinate(-1, 0, 0)
      )
  
  
    Particle(
      1,
      Coordinate(4, 0, 0),
      Coordinate(0, 0, 0),
      Coordinate(-2, 0, 0)
    ).next shouldBe
      Particle(
        1,
        Coordinate(2, 0, 0),
        Coordinate(-2, 0, 0),
        Coordinate(-2, 0, 0)
      )
  }
  
  test("can collide particles") {
    countSurvivingParticles(
      parseLines(
        """p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
          |p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
          |p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
          |p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>""".stripMargin.lines
      )
    ) shouldBe 1
  }
}
(new Day20Part2Test).execute()
```

I can now run the simulation and try the answer. 

```tut:book
countSurvivingParticles(input)
```

Since that was counted as correct, I'll leave today there.
