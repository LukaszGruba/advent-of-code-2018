package com.lukgru.algo.advent2018.day17

import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class ReservoirResearchTest extends FunSuite {

  test("should solve simple example") {
    //given
    val input = List(
      "x=495, y=2..7",
      "y=7, x=495..501",
      "x=501, y=3..7",
      "x=498, y=2..4",
      "x=506, y=1..2",
      "x=498, y=10..13",
      "x=504, y=10..13",
      "y=13, x=498..504"
    )

    //when
    val solution = ReservoirResearch.solvePart1(500)(input)

    //then
    assert(solution == 57)
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day17-input")

    //when
    val solution = ReservoirResearch.solvePart1(500)(input)

    //then
    assert(solution == 33004)
  }
}
