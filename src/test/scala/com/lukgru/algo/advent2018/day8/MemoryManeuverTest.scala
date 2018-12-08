package com.lukgru.algo.advent2018.day8

import com.lukgru.algo.advent2018.day8.MemoryManeuver.Node
import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class MemoryManeuverTest extends FunSuite {

  test("should parse a simple tree") {
    //given
    val data = List(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)

    //when
    val (tree, _) = MemoryManeuver.parseTree(data)

    //then
    assert(tree ==
      Node(List(
        Node(List.empty, List(10, 11, 12)),
        Node(List(
          Node(List.empty, List(99))
        ), List(2))
      ), List(1, 1, 2)))
  }

  test("should traverse a tree") {
    //given
    val tree =
      Node(List(
        Node(List.empty, List(10, 11, 12)),
        Node(List(
          Node(List.empty, List(99))
        ), List(2))
      ), List(1, 1, 2))

    //when
    val result = MemoryManeuver.traverseTree(_.metadata.sum, (a: Int, b: Int) => a + b)(tree)

    //then
    assert(result == 138)
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day8-input").head

    //when
    val result = MemoryManeuver.solvePart1(input)

    //then
    assert(result == 47464)
  }

  test("should solve part 2") {
    //given
    val input = InputLoader.loadLines("day8-input").head

    //when
    val result = MemoryManeuver.solvePart2(input)

    //then
    assert(result == 23054)
  }

}
