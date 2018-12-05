package com.lukgru.algo.advent2018.day5

import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class AlchemicalReductionTest extends FunSuite {

  test("should react") {
    assert(AlchemicalReduction.isReacting('a', 'A'))
    assert(AlchemicalReduction.isReacting('B', 'b'))
  }

  test("should not react") {
    assert(!AlchemicalReduction.isReacting('a', 'a'))
    assert(!AlchemicalReduction.isReacting('A', 'A'))
    assert(!AlchemicalReduction.isReacting('a', 'B'))
  }

  test("should reduce fully") {
    //given
    val input = "abBAAcCcDaAaaaaAbcdefFEDCBx"

    //when
    val reduced = AlchemicalReduction.reduce(input)

    //then
    assert(reduced == "AcDaaax")
  }

  test("should remove specific unit regardless of polarity") {
    //given
    val polymer = "dabAcCaCBAcCcaDA"
    val unit = 'c'

    //when
    val remainingPolymer = AlchemicalReduction.removeUnit(polymer)(unit)

    assert(remainingPolymer == "dabAaBAaDA")
  }

  test("should find optimal reduction") {
    //given
    val polymer = "dabAcCaCBAcCcaDA"

    //when
    val optimalPolymer = AlchemicalReduction.optimizedReduce(polymer)

    //then
    assert(optimalPolymer == "daDA")
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day5-input").head

    //when
    val solution = AlchemicalReduction.reduce(input).length

    //then
    assert(solution == 9202)
  }

  test("should solve part 2") {
    //given
    val input = InputLoader.loadLines("day5-input").head

    //when
    val solution = AlchemicalReduction.optimizedReduce(input).length

    //then
    assert(solution == 6394)
  }


}
