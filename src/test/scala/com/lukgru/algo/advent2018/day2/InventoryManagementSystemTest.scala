package com.lukgru.algo.advent2018.day2

import com.lukgru.algo.advent2018.day2.InventoryManagementSystem.containsDuplicates

class InventoryManagementSystemTest extends org.scalatest.FunSuite {

  test("should detect 3 duplicates") {
    //given
    val input = "abcdcecf"

    //when
    val contains3s = containsDuplicates(3)(input)

    //then
    assert(contains3s)
  }

  test("should not detect 4 duplicates if contains 3") {
    //given
    val input = "abcdcecf"

    //when
    val contains4s = containsDuplicates(4)(input)

    //then
    assert(!contains4s)
  }

  test("should not detect 2 duplicates if contains 3") {
    //given
    val input = "abcdcecf"

    //when
    val contains2s = containsDuplicates(2)(input)

    //then
    assert(!contains2s)
  }
}
