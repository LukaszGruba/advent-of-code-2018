package com.lukgru.algo.advent2018.day23

import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class ExperimentalEmergencyTeleportationTest extends FunSuite {

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day23-input")

    //when
    val solution = ExperimentalEmergencyTeleportation.countNanobotsInRangeOfTheStrongestOne(input)

    //then
    assert(solution == 721)
  }

  test("should solve part 2 for simple example") {
    //given
    val lines = List(
      "pos=<10,12,12>, r=2",
      "pos=<12,14,12>, r=2",
      "pos=<16,12,12>, r=4",
      "pos=<14,14,14>, r=6",
      "pos=<50,50,50>, r=200",
      "pos=<10,10,10>, r=5"
    )

    //when
    val solution = ExperimentalEmergencyTeleportation.solvePart2(lines)

    //then
    assert(solution == 36)
  }

}
