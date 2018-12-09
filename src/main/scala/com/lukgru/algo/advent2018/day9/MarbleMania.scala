package com.lukgru.algo.advent2018.day9

import scala.annotation.tailrec

object MarbleMania {

  case class Elf(id: Int, marbles: List[Marble] = List.empty)

  case class Marble(no: Int)

  def nextElf(queue: List[Elf]): (Elf, List[Elf]) = (queue.head, queue.tail)

  def takeMarble(marbles: List[Marble]): (Marble, List[Marble]) = (marbles.head, marbles.tail)

  def goToTheEnd(elf: Elf, restOfQueue: List[Elf]): List[Elf] = restOfQueue :+ elf

  def createQueue(numberOfElves: Int): List[Elf] =
    Stream.from(1)
      .map(Elf(_))
      .take(numberOfElves)
      .toList

  def createMarbles(numberOfMarbles: Int): List[Marble] =
    Stream.from(0)
      .map(Marble)
      .take(numberOfMarbles)
      .toList

  def playMarbles(numberOfPlayers: Int, lastMarbleValue: Int): Int = {
    val elves = createQueue(numberOfPlayers)
    val marbles = createMarbles(lastMarbleValue)

    val (firstMarble, rest) = takeMarble(marbles)
    val circle = List(firstMarble)

    val winner = play(circle, elves, rest)
    winner.marbles
      .map(_.no)
      .sum
  }

  def placeMarbleInTheCircle(marble: Marble, circle: List[Marble]): List[Marble] =
    circle match {
      case only :: Nil => marble :: only :: Nil
      case curr :: fst :: rest => (marble :: rest) ++ List(curr, fst)
    }

  def popOut7thMarbleBefore(circle: List[Marble]): (Marble, List[Marble]) = {
    val (newTail, newInit) = circle.splitAt(circle.length - 7)
    (newInit.head, newInit.tail ++ newTail)
  }

  def winMarble(currentElf: Elf, currentCircle: List[Marble], marble: Marble): (Elf, List[Marble]) = {
    val (additionalMarble, newCircle) = popOut7thMarbleBefore(currentCircle)
    val elf = currentElf.copy(marbles = currentElf.marbles :+ marble :+ additionalMarble)
    (elf, newCircle)
  }

  @tailrec
  def play(circle: List[Marble], elves: List[Elf], remainingMarbles: List[Marble]): Elf =
    remainingMarbles match {
      case Nil =>
        elves.maxBy { elf =>
          elf.marbles
            .map(_.no)
            .sum
        }
      case _ =>
        val (marble, restOfMarbles) = takeMarble(remainingMarbles)
        val (currentElf, restOfTheQueue) = nextElf(elves)
        val (elf, newCircle) =
          if (marble.no % 23 == 0) {
            winMarble(currentElf, circle, marble)
          } else {
            (currentElf, placeMarbleInTheCircle(marble, circle))
          }
        play(newCircle, goToTheEnd(elf, restOfTheQueue), restOfMarbles)
    }

  def main(args: Array[String]): Unit = {
    val numberOfPlayers = 429
    val lastMarbleValue = 70901

    val solution1 = playMarbles(numberOfPlayers, lastMarbleValue)
    println(solution1)
  }

}
