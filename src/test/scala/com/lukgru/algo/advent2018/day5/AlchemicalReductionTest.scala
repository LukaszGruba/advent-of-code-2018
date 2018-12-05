package com.lukgru.algo.advent2018.day5

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

}
