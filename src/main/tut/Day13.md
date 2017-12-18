# Stream Processing

A solution for [Advent of Code 2017 - Day 13](http://adventofcode.com/2017/day/13)

## Part 1

Today's input is a set of patrolling bots. 

```tut:book
case class Layer(depth: Int, range: Int)

private val LineMatcher = "(\\d+): (\\d+)".r

def parseLines(lines: TraversableOnce[String]): Seq[Layer] =
  lines.toSeq.collect { case LineMatcher(d, r) => Layer(d.toInt, r.toInt) }
```

## Part 2
