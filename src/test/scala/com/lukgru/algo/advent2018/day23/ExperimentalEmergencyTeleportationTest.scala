package com.lukgru.algo.advent2018.day23

import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class ExperimentalEmergencyTeleportationTest extends FunSuite {

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day23-input")

    //when
    val solution = ExperimentalEmergencyTeleportation.solvePart1(input)

    //then
    assert(solution == 721)
  }

}
