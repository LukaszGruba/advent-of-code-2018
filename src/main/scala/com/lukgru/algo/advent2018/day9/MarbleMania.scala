package com.lukgru.algo.advent2018.day9

import java.util.Date

import scala.annotation.tailrec

object MarbleMania {

  case class Elf(id: Int, marbles: Vector[Marble] = Vector.empty)

  case class Marble(no: Int)

  def nextElf(queue: Vector[Elf]): (Elf, Vector[Elf]) = (queue.head, queue.tail)

  def takeMarble(marbles: Vector[Marble]): (Marble, Vector[Marble]) = (marbles.head, marbles.tail)

  def goToTheEnd(elf: Elf, restOfQueue: Vector[Elf]): Vector[Elf] = restOfQueue :+ elf

  def createQueue(numberOfElves: Int): Vector[Elf] =
    Stream.from(1)
      .map(Elf(_))
      .take(numberOfElves)
      .toVector

  def createMarbles(numberOfMarbles: Int): Vector[Marble] =
    Stream.from(0)
      .map(Marble)
      .take(numberOfMarbles)
      .toVector

  def playMarbles(numberOfPlayers: Int, lastMarbleValue: Int): Long = {
    val elves = createQueue(numberOfPlayers)
    val marbles = createMarbles(lastMarbleValue)

    val (firstMarble, rest) = takeMarble(marbles)
    val circle = Vector(firstMarble)

    val winner = play(circle, elves, rest)
    winner.marbles
      .map(_.no.toLong)
      .sum
  }

  def placeMarbleInTheCircle(marble: Marble, circle: Vector[Marble]): Vector[Marble] =
    circle match {
      case Vector(only) => Vector(marble, only)
      case curr +: fst +: rest => (marble +: rest) ++ Vector(curr, fst)
    }

  def popOut7thMarbleBefore(circle: Vector[Marble]): (Marble, Vector[Marble]) = {
    val (newTail, newInit) = circle.splitAt(circle.length - 7)
    (newInit.head, newInit.tail ++ newTail)
  }

  def winMarble(currentElf: Elf, currentCircle: Vector[Marble], marble: Marble): (Elf, Vector[Marble]) = {
    val (additionalMarble, newCircle) = popOut7thMarbleBefore(currentCircle)
    val elf = currentElf.copy(marbles = currentElf.marbles :+ marble :+ additionalMarble)
    (elf, newCircle)
  }

  @tailrec
  def play(circle: Vector[Marble], elves: Vector[Elf], remainingMarbles: Vector[Marble]): Elf =
    remainingMarbles match {
      case Vector() =>
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

    val solution2 = playMarbles(numberOfPlayers, 100 * lastMarbleValue)
    println(solution2)
  }

}
