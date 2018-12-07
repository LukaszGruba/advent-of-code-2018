package com.lukgru.algo.advent2018.day6

import org.scalatest.FunSuite

class ChronalCoordinatesTest extends FunSuite {

  test("should work for simple example") {
    //given
    val input = List(
      "101, 101",
      "101, 106",
      "108, 103",
      "103, 104",
      "105, 105",
      "108, 109"
    )

    //when
    val solution = ChronalCoordinates.solvePart1(input)

    //then
    assert(solution == 17)
  }
}
