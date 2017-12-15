# High-Entropy Passphrases

A solution for [Advent of Code 2017 - Day 4](http://adventofcode.com/2017/day/4)

## Part 1

Today's goal is to throw out a bunch of insecure passphrases. Specifically those
with duplicate words. I can see sets being my friend.

```tut:book
def iterSecure(words: List[String], seen: Set[String] = Set.empty): Boolean = (
  words match {
    case Nil => true
    case word :: _ if seen.contains(word) => false
    case word :: rest => iterSecure(rest, seen + word)
  }
)
 
def isSecure(passphrase: String): Boolean = iterSecure(passphrase.split(" ").toList, Set.empty)

def countSecure(passphrases: Seq[String]): Int = passphrases.count(isSecure)
```

The recursive `iterSecure` was originally written inside isSecure as just iter.
A pattern I often use that keeps the implementation-only arguments in the 
recursion hidden from the outside world. It then turned out part two needed to 
call it in a subtly different way, so I lifted it out to a def.

There were some examples to use as test cases.

```tut:book
import org.scalatest.{FunSuite, Matchers}

class Day4Part1Test extends FunSuite with Matchers {

  test("can test a phrase is secure") {
    isSecure("aa bb cc dd ee") shouldBe true
    isSecure("aa bb cc dd aa") shouldBe false
    isSecure("aa bb cc dd aaa") shouldBe true
    isSecure("ab") shouldBe true
    isSecure("a b") shouldBe true
    isSecure("a a") shouldBe false
    isSecure("a a a") shouldBe false

    isSecure("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp") shouldBe true
    isSecure("xmuf znkhaes pggrlp zia znkhaes znkhaes") shouldBe false
    isSecure("nti rxr bogebb zdwrin") shouldBe true
    isSecure("sryookh unrudn zrkz jxhrdo gctlyz") shouldBe true
    isSecure("bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken") shouldBe true
    isSecure("flawt cpott xth ucwgg xce jcubx wvl qsysa nlg") shouldBe true

  }

  test("can count secure phrases") {
    countSecure(Seq("aa bb")) shouldBe 1
    countSecure(Seq("aa bb", "aa bb")) shouldBe 2
    countSecure(Seq("aa aa", "aa aa")) shouldBe 0
    countSecure(Seq("aa aa", "aa bb")) shouldBe 1
    countSecure(Seq("aa bb")) shouldBe 1

    countSecure(Seq(
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa",
      "ab",
      "a b",
      "a a",
      "a a a"
    )) shouldBe 4

    countSecure(Seq(
      "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp",
      "xmuf znkhaes pggrlp zia znkhaes znkhaes",
      "nti rxr bogebb zdwrin",
      "sryookh unrudn zrkz jxhrdo gctlyz",
      "bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken",
      "flawt cpott xth ucwgg xce jcubx wvl qsysa nlg"
    )) shouldBe 5

  }
}

(new Day4Part1Test).execute()
```

I can now run against the test input

```tut:book
import scala.io.Source
val input = Source.fromResource("day4input.txt").getLines().toSeq

countSecure(input)
```

## Part 2

For part two, the validator now needs to reject duplicate anagrams as well. This
can be achieved by sorting each word alphabetically, which reduces the problem
to the same one solved in part one.

```tut:book
def isReallySecure(passphrase: String): 
  Boolean = iterSecure(passphrase.split(" ").toList.map(s => s.sorted))

def countReallySecure(passphrases: Seq[String]): 
  Int = passphrases.count(isReallySecure)
```

There are some more test cases, and some of the previous ones can be reused with
slightly different answers

```tut:book
class Day4Part2Test extends FunSuite with Matchers {
  test("can test a phrase is *really* secure") {

    isReallySecure("aa bb cc dd ee") shouldBe true
    isReallySecure("aa bb cc dd aa") shouldBe false
    isReallySecure("aa bb cc dd aaa") shouldBe true
    isReallySecure("ab") shouldBe true
    isReallySecure("ab ba") shouldBe false
    isReallySecure("a b") shouldBe true
    isReallySecure("a a") shouldBe false
    isReallySecure("a a a") shouldBe false


    isReallySecure("abcde fghij") shouldBe true
    isReallySecure("abcde xyz ecdab") shouldBe false
    isReallySecure("a ab abc abd abf abj") shouldBe true
    isReallySecure("iiii oiii ooii oooi oooo") shouldBe true
    isReallySecure("oiii ioii iioi iiio") shouldBe false


    isReallySecure("sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp") shouldBe true
    isReallySecure("xmuf znkhaes pggrlp zia znkhaes znkhaes") shouldBe false
    isReallySecure("nti rxr bogebb zdwrin") shouldBe true
    isReallySecure("sryookh unrudn zrkz jxhrdo gctlyz") shouldBe true
    isReallySecure("bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken") shouldBe false
    isReallySecure("flawt cpott xth ucwgg xce jcubx wvl qsysa nlg") shouldBe true
  }

  test("can count *really* secure phrases") {

    countReallySecure(Seq(
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa",
      "ab",
      "ab ba",
      "a b",
      "a a",
      "a a a"
    )) shouldBe 4

    countReallySecure(Seq(
      "abcde fghij",
      "abcde xyz ecdab",
      "a ab abc abd abf abj",
      "iiii oiii ooii oooi oooo",
      "oiii ioii iioi iiio"
    )) shouldBe 3

    countReallySecure(Seq(
      "sayndz zfxlkl attjtww cti sokkmty brx fhh suelqbp",
      "xmuf znkhaes pggrlp zia znkhaes znkhaes",
      "nti rxr bogebb zdwrin",
      "sryookh unrudn zrkz jxhrdo gctlyz",
      "bssqn wbmdc rigc zketu ketichh enkixg bmdwc stnsdf jnz mqovwg ixgken",
      "flawt cpott xth ucwgg xce jcubx wvl qsysa nlg"
    )) shouldBe 4
  }
}

(new Day4Part2Test).execute()
```

All that is left is to run with the puzzle input

```tut:book
countReallySecure(input)
```
