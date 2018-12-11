package com.lukgru.algo.advent2018.day11

import org.scalatest.FunSuite

class ChronalChargeTest extends FunSuite {

  test("should calc power level") {
    //given
    val gridSerialNumber = 8
    val x = 3
    val y = 5

    //when
    val powerLevel = ChronalCharge.powerLevel(gridSerialNumber)(x, y)

    //then
    assert(powerLevel == 4)
  }

  test("should solve part 1") {
    //when
    val maxSquare = ChronalCharge.findMaxPowerSquareCoords(3)(4455)

    //then
    assert(maxSquare == (21, 54))
  }

}
