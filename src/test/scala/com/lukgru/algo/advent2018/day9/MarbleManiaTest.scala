package com.lukgru.algo.advent2018.day9

import com.lukgru.algo.advent2018.day9.MarbleMania.{Elf, Marble}
import org.scalatest.FunSuite

class MarbleManiaTest extends FunSuite {

  test("should pop next elf") {
    //given
    val currentQueue = Vector(Elf(0), Elf(1), Elf(2), Elf(3))

    //when
    val (currentElf, newQueue) = MarbleMania.nextElf(currentQueue)

    //then
    assert(currentElf == Elf(0))
    assert(newQueue == Vector(Elf(1), Elf(2), Elf(3)))
  }

  test("should send elf to the end") {
    //given
    val currentElf = Elf(0)
    val restOfTheQueue = Vector(Elf(1), Elf(2), Elf(3))

    //when
    val queue = MarbleMania.goToTheEnd(currentElf, restOfTheQueue)

    //then
    assert(queue == Vector(Elf(1), Elf(2), Elf(3), Elf(0)))
  }

  test("should put 5 marbles into the circle") {
    //given
    val marbles = Vector(Marble(1), Marble(2), Marble(3), Marble(4), Marble(5))
    val circle = Vector(Marble(0))

    //when
    val newCircle = marbles.foldLeft(circle) { (circ, marble) =>
      MarbleMania.placeMarbleInTheCircle(marble, circ)
    }

    //then
    assert(newCircle == Vector(Marble(5), Marble(1), Marble(3), Marble(0), Marble(4), Marble(2)))
  }

  test("should pop out 7th marble before and create new circle") {
    //givne
    val circle = Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10).map(Marble)

    //when
    val (seventh, newCircle) = MarbleMania.popOut7thMarbleBefore(circle)

    //then
    assert(seventh == Marble(4))
    val expectedCircle = Vector(5, 6, 7, 8, 9, 10, 0, 1, 2, 3).map(Marble)
    assert(newCircle == expectedCircle)
  }

  test("should work for 10 players and last marble with value of 1618") {
    //given
    val numberOfPlayers = 10
    val lastMarbleValue = 1618

    //when
    val winnersScore = MarbleMania.playMarbles(numberOfPlayers, lastMarbleValue)

    //then
    assert(winnersScore == 8317)
  }

  test("should solve part 1") {
    //given
    val numberOfPlayers = 429
    val lastMarbleValue = 70901

    //when
    val winnersScore = MarbleMania.playMarbles(numberOfPlayers, lastMarbleValue)

    //then
    assert(winnersScore == 399645)
  }

  test("should solve part 2") {
    //given
    val numberOfPlayers = 429
    val lastMarbleValue = 70901

    //when
    val winnersScore = MarbleMania.playMarbles(numberOfPlayers, 100 * lastMarbleValue)

    //then
    assert(winnersScore == 3352507536L)
  }

}
