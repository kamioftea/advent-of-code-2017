# Recursive Circus

A solution for [Advent of Code 2017 - Day 7](http://adventofcode.com/2017/day/7)

## Part 1

Today the puzzle scenario is a recursive data structure, realised as a wobbling
tower of discs. The main aim of part one is to make sense of the way this 
structure is represented. At least, sense enough to determine its root.

Since the nodes of the structure are labeled with strings, I can store this as
a map of string to a case class to store a reference to the parent, and to any
children. Using the string label, rather than a hard reference to the other node
makes the structure much easier to build one node at a time. To make things a
bit easier to keep track of, I'll use a type alias to mark when a string is 
being used as a reference to a node.

```tut:book
type NodeRef = String
case class Node(name: String,
                weight: Option[Int] = None,
                parent: Option[NodeRef] = None,
                children: Set[NodeRef] = Set.empty)

val LineMatcher = "([a-z]+) \\((\\d+)\\)(?: -> ((?:[a-z]+(?:, )?)+))?".r

def parseChildList(children: String): Set[NodeRef] = (
  if (children == null) Set.empty
  else children.split(", ").toSet
)

def parseInput(lines: TraversableOnce[String]): Map[NodeRef, Node] = (
  lines.foldLeft(Map.empty[String, Node]) {
    case (map, LineMatcher(name, weight, children)) =>
      val node =
        map.getOrElse(name, Node(name))
          .copy(
            weight = Some(weight.toInt),
            children = parseChildList(children)
          )
      node.children
        .foldLeft(map.updated(node.name, node)) {
          case (newMap, nodeRef) =>
            newMap.updated(
              nodeRef,
              newMap.getOrElse(nodeRef, Node(nodeRef))
                .copy(parent = Some(node.name))
            )
        }
    case (map, _) => map
  }
)
```

Once the data has been corralled into something usable, getting the actual 
answer is fairly simple.

```tut:book
def findRoot(nodes: TraversableOnce[Node]): Option[Node] = 
    nodes.find(n => n.parent.isEmpty)
``` 

There is only one example in the puzzle description, but being a recursive
structure we can break it down into multiple sub tests. They are unfortunately
quite verbose.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day7Part1Test extends FunSuite with Matchers {
  def sample =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)""".stripMargin.lines

  def subSample1 =
    """xhth (57)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |cntj (57)""".stripMargin.lines

  def subSample2 =
    """pbga (67)
      |havc (66)
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq""".stripMargin.lines

  def subSample3 =
    """ugmlq (68) -> gyxo, ebii
      |ebii (61)
      |gyxo (61)""".stripMargin.lines


  test("can parse input lines") {
    val parsedSubSample1: Map[String, Node] = parseInput(subSample1)

    parsedSubSample1.size shouldBe 4
    parsedSubSample1.get("xhth") shouldBe Some(Node("xhth", Some(57), Some("fwft")))
    parsedSubSample1.get("ktlj") shouldBe Some(Node("ktlj", Some(57), Some("fwft")))
    parsedSubSample1.get("fwft") shouldBe Some(Node("fwft", Some(72), None, Set("xhth", "ktlj", "cntj")))
    parsedSubSample1.get("cntj") shouldBe Some(Node("cntj", Some(57), Some("fwft")))

    val parsedSubSample2: Map[String, Node] = parseInput(subSample2)

    parsedSubSample2.size shouldBe 4
    parsedSubSample2.get("pbga") shouldBe Some(Node("pbga", Some(67), Some("padx")))
    parsedSubSample2.get("havc") shouldBe Some(Node("havc", Some(66), Some("padx")))
    parsedSubSample2.get("padx") shouldBe Some(Node("padx", Some(45), None, Set("pbga", "havc", "qoyq")))
    parsedSubSample2.get("qoyq") shouldBe Some(Node("qoyq", Some(66), Some("padx")))

    val parsedSubSample3: Map[String, Node] = parseInput(subSample3)

    parsedSubSample3.size shouldBe 3
    parsedSubSample3.get("ebii") shouldBe Some(Node("ebii", Some(61), Some("ugmlq")))
    parsedSubSample3.get("gyxo") shouldBe Some(Node("gyxo", Some(61), Some("ugmlq")))
    parsedSubSample3.get("ugmlq") shouldBe Some(Node("ugmlq", Some(68), None, Set("ebii", "gyxo")))

    val parsedSample: Map[String, Node] = parseInput(sample)

    parsedSample.size shouldBe 13
    parsedSample.get("xhth") shouldBe Some(Node("xhth", Some(57), Some("fwft")))
    parsedSample.get("ktlj") shouldBe Some(Node("ktlj", Some(57), Some("fwft")))
    parsedSample.get("fwft") shouldBe Some(Node("fwft", Some(72), Some("tknk"), Set("xhth", "ktlj", "cntj")))
    parsedSample.get("cntj") shouldBe Some(Node("cntj", Some(57), Some("fwft")))
    parsedSample.get("pbga") shouldBe Some(Node("pbga", Some(66), Some("padx")))
    parsedSample.get("havc") shouldBe Some(Node("havc", Some(66), Some("padx")))
    parsedSample.get("padx") shouldBe Some(Node("padx", Some(45), Some("tknk"), Set("pbga", "havc", "qoyq")))
    parsedSample.get("qoyq") shouldBe Some(Node("qoyq", Some(66), Some("padx")))
    parsedSample.get("ebii") shouldBe Some(Node("ebii", Some(61), Some("ugml")))
    parsedSample.get("gyxo") shouldBe Some(Node("gyxo", Some(61), Some("ugml")))
    parsedSample.get("ugml") shouldBe Some(Node("ugml", Some(68), Some("tknk"), Set("ebii", "gyxo", "jptl")))
    parsedSample.get("jptl") shouldBe Some(Node("jptl", Some(61), Some("ugml")))
    parsedSample.get("tknk") shouldBe Some(Node("tknk", Some(41), None, Set("ugml", "padx", "fwft")))
  }

  test("can find structure root") {
    findRoot(parseInput(subSample1).map { case (_, v) => v }).map(n => n.name) shouldBe Some("fwft")
    findRoot(parseInput(subSample2).values).map(n => n.name) shouldBe Some("padx")
    findRoot(parseInput(subSample3).values).map(n => n.name) shouldBe Some("ugmlq")
    findRoot(parseInput(sample).values).map(n => n.name) shouldBe Some("tknk")
  }
}

(new Day7Part1Test).execute()
```

I can now parse the input file and find the solution:

```tut:book
import scala.io.Source
val input = parseInput(Source.fromResource("day7input.txt").getLines())

findRoot(input.values).get.name
```

## Part 2

Now I actually have to do some analysis of the data structure. Each node has a
weight. The structure should have an invariant such that for each node, each of 
its sub-trees has the same weight. However there is one node that is violating
this. The task is to find that node, and return the weight it **should** be to
satisfy the invariant.

Since I already have a way to find the root, I can start there, but to calculate 
the sub-tree weights I need to start at the roots. So the recursive function 
first recurses until it hits leaves, which have an empty group, and just return
their weight.

Once we have data for all the children, if any of them don't have the same 
weight we have found the corrupt node, and can just pass that up the tree. 
For this reason the recursion returns an optional node and corrected weight pair.

If this is set for any of the nodes, it can be returned, otherwise if one child
has a different weight, calculate the difference and set the data to be passed 
down.

```tut:book
def findMismatchedNode(nodes: Map[NodeRef, Node]): Option[(Node, Int)] = {
  def iter(node: Node): (Int, Option[(Node, Int)]) = {
    val childData = node.children.toList.map(
      c_ref => (c_ref, iter(nodes(c_ref)))
    )

    val totalWeight =
      node.weight.getOrElse(0) + childData.map { case (_, (w, _)) => w }.sum

    val groups = childData.groupBy { case (_, (w, _)) => w }
    (
      totalWeight,
      groups
        .find { case (_, ns) => ns.lengthCompare(1) == 0 }
        .collect {
          case (cW, (c_ref, (_, maybeNode)) :: _) =>
            maybeNode.getOrElse({
              val childNode = nodes(c_ref)
              (
                childNode,
                groups.find {
                  case (_, ns) => ns.lengthCompare(1) > 0
                }.map {
                  case (w, _) => childNode.weight.getOrElse(0) - cW + w
                }.getOrElse(0)
              )
            })
        }
    )
  }
  
  val root = findRoot(nodes.values)
  
  root.flatMap { r =>
    iter(r) match {
      case (_, maybeMismatch) => maybeMismatch
    }
  }
}
```

The testing can reuse most of the data from the previous tests.

```tut:book
class Day7Part2Test extends FunSuite with Matchers {
  def sample =
    """pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)""".stripMargin.lines

  def subSample2 =
    """pbga (67)
      |havc (66)
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq""".stripMargin.lines

  test("testFindMismatchedWeight") {
    findMismatchedNode(parseInput(subSample2)) shouldBe
    Some((Node("pbga", Some(67), Some("padx")), 66))

    findMismatchedNode(parseInput(sample)) shouldBe
    Some((Node("ugml", Some(68), Some("tknk"), Set("ebii", "gyxo", "jptl")), 60))
  }
}

(new Day7Part2Test).execute()
```

And finally I can now extract the puzzle solution from the data returned:

```tut:book
findMismatchedNode(input).get._2
```
