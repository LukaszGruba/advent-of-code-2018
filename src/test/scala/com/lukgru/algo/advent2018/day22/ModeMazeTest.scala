package com.lukgru.algo.advent2018.day22

import com.lukgru.algo.advent2018.day22.ModeMaze.Position
import org.scalatest.FunSuite

class ModeMazeTest extends FunSuite {

  test("should solve for simple example"){
    //given
    val depth = 510
    val entrance = Position(0, 0)
    val target = Position(10, 10)

    //when
    val solution = ModeMaze.solvePart1(depth, entrance, target)

    //then
    assert(solution == 114)
  }

  test("should solve part 1") {
    //given
    val depthP = 7740
    val caveEntryPosP = Position(0, 0)
    val targetPosP = Position(12, 763)

    //when
    val solution = ModeMaze.solvePart1(depthP, caveEntryPosP, targetPosP)

    //then
    assert(solution == 9899)
  }

}
