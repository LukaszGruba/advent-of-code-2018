package com.lukgru.algo.advent2018.day25

import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class FourDimentionalAdventureTest extends FunSuite {

  test("should solve part 1 for simple example 1") {
    //given
    val lines = List(
      "0,0,0,0",
      "3,0,0,0",
      "0,3,0,0",
      "0,0,3,0",
      "0,0,0,3",
      "0,0,0,6",
      "9,0,0,0",
      "12,0,0,0"
    )

    //when
    val solution = FourDimentionalAdventure.solvePart1(lines)

    //then
    assert(solution == 2)
  }

  test("should solve part 1 for simple example 2") {
    //given
    val lines = List(
      "-1,2,2,0",
      "0,0,2,-2",
      "0,0,0,-2",
      "-1,2,0,0",
      "-2,-2,-2,2",
      "3,0,2,-1",
      "-1,3,2,2",
      "-1,0,-1,0",
      "0,2,1,-2",
      "3,0,0,0"
    )

    //when
    val solution = FourDimentionalAdventure.solvePart1(lines)

    //then
    assert(solution == 4)
  }

  test("should solve part 1 for simple example 3") {
    //given
    val lines = List(
      "1,-1,0,1",
      "2,0,-1,0",
      "3,2,-1,0",
      "0,0,3,1",
      "0,0,-1,-1",
      "2,3,-2,0",
      "-2,2,0,0",
      "2,-2,0,-1",
      "1,-1,0,-1",
      "3,2,0,2"
    )

    //when
    val solution = FourDimentionalAdventure.solvePart1(lines)

    //then
    assert(solution == 3)
  }


  test("should solve part 1 for simple example 4") {
    //given
    val lines = List(
      "1,-1,-1,-2",
      "-2,-2,0,1",
      "0,2,1,3",
      "-2,3,-2,1",
      "0,2,3,-2",
      "-1,-1,1,-2",
      "0,-2,-1,0",
      "-2,2,3,-1",
      "1,2,2,0",
      "-1,-2,0,-2"
    )

    //when
    val solution = FourDimentionalAdventure.solvePart1(lines)

    //then
    assert(solution == 8)
  }

  test("should solve part 1") {
    //given
    val lines = InputLoader.loadLines("day25-input")

    //when
    val solution = FourDimentionalAdventure.solvePart1(lines)

    //then
    assert(solution == 350)
  }

}
