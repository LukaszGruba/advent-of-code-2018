package com.lukgru.algo.advent2018.day18

import com.lukgru.algo.advent2018.utils.InputLoader
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

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day18-input")

    //when
    val solution = SettlersOfTheNorthPole.solvePart1(input)

    //then
    assert(solution == 480150)
  }

}
