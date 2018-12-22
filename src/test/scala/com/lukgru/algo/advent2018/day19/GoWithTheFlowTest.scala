package com.lukgru.algo.advent2018.day19

import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class GoWithTheFlowTest extends FunSuite {

  test("should execute simple example") {
    //given
    val input = List(
      "#ip 0",
      "seti 5 0 1",
      "seti 6 0 2",
      "addi 0 1 0",
      "addr 1 2 3",
      "setr 1 0 0",
      "seti 8 0 4",
      "seti 9 0 5"
    )

    //when
    val solution = GoWithTheFlow.executeProgram(input)

    //then
    assert(solution == 6)
  }

  test("should solve part 1") {
    //given
    val program = InputLoader.loadLines("day19-input")

    //when
    val result = GoWithTheFlow.executeProgram(program)

    //then
    assert(result == 2520)
  }

}
