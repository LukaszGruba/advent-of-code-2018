package com.lukgru.algo.advent2018.day7

import com.lukgru.algo.advent2018.day7.TheSumOfItsParts.{Node, Rule}
import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class TheSumOfItsPartsTest extends FunSuite {

  test("should parse rules correctly") {
    //given
    val sampleInput = List(
      "Step C must be finished before step A can begin.",
      "Step C must be finished before step F can begin.",
      "Step A must be finished before step B can begin.",
      "Step A must be finished before step D can begin.",
      "Step B must be finished before step E can begin.",
      "Step D must be finished before step E can begin.",
      "Step F must be finished before step E can begin."
    )

    //when
    val rules = TheSumOfItsParts.parseRules(sampleInput)

    //then
    assert(rules == List(
      Rule("C", "A"),
      Rule("C", "F"),
      Rule("A", "B"),
      Rule("A", "D"),
      Rule("B", "E"),
      Rule("D", "E"),
      Rule("F", "E")
    ))
  }

  test("should construct graph") {
    //given
    val rules = List(
      Rule("C", "A"),
      Rule("C", "F"),
      Rule("A", "B"),
      Rule("A", "D"),
      Rule("B", "E"),
      Rule("D", "E"),
      Rule("F", "E")
    )

    //when
    val graph = TheSumOfItsParts.constructGraph(rules)

    //then
    val a = Node("A", Set("C"), Set("B", "D"))
    val b = Node("B", Set("A"), Set("E"))
    val c = Node("C", Set.empty, Set("A", "F"))
    val d = Node("D", Set("A"), Set("E"))
    val e = Node("E", Set("B", "D", "F"), Set.empty)
    val f = Node("F", Set("C"), Set("E"))

    assert(graph == Set(a, b, c, d, e, f))
  }

  test("should work for simple example") {
    //given
    val input = List(
      "Step C must be finished before step A can begin.",
      "Step C must be finished before step F can begin.",
      "Step A must be finished before step B can begin.",
      "Step A must be finished before step D can begin.",
      "Step B must be finished before step E can begin.",
      "Step D must be finished before step E can begin.",
      "Step F must be finished before step E can begin."
    )

    //when
    val solution = TheSumOfItsParts.traverseGraph(input)

    //then
    assert(solution == "CABDFE")
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day7-input")

    //when
    val solution = TheSumOfItsParts.traverseGraph(input)

    //then
    assert(solution == "CFGHAEMNBPRDISVWQUZJYTKLOX")
  }

}
