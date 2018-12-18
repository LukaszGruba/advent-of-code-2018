package com.lukgru.algo.advent2018.day18

import org.scalatest.FunSuite

class SettlersOfTheNorthPoleTest extends FunSuite {

  test("should solve simple example") {
    //given
    val input = List(
      ".#.#...|#.",
      ".....#|##|",
      ".|..|...#.",
      "..|#.....#",
      "#.#|||#|#|",
      "...#.||...",
      ".|....|...",
      "||...#|.#|",
      "|.||||..|.",
      "...#.|..|."
    )

    //when
    val solution = SettlersOfTheNorthPole.solvePart1(input)

    //then
    assert(solution == 1147)
  }

}
