package com.lukgru.algo.advent2018.day12

import com.lukgru.algo.advent2018.day12.SubterraneanSustainability._
import org.scalatest.FunSuite

class SubterraneanSustainabilityTest extends FunSuite {

  test("should trim generation properly") {
    //given
    val gen = parseState("...#..#.#..##......###...###...........")


    //when
    val newGen = trimGeneration(gen)

    //then
    assert(formatGeneration(newGen) == "....#..#.#..##......###...###....")
  }

  test("should grow generation") {
    //given
    val gen = parseState("...#..#.#..##......###...###...........")
    val rules = parseRules(List(
      "...## => #",
      "..#.. => #",
      ".#... => #",
      ".#.#. => #",
      ".#.## => #",
      ".##.. => #",
      ".#### => #",
      "#.#.# => #",
      "#.### => #",
      "##.#. => #",
      "##.## => #",
      "###.. => #",
      "###.# => #",
      "####. => #"
    ))

    //when
    val newGen = growGeneration(rules)(gen)
    val str = formatGeneration(trimGeneration(newGen))

    //then
    val expected = "....#...#....#.....#..#..#..#...."
    assert(str == expected)
  }

  test("should solve example") {
    //given
    val lines = List(
      "initial state: #..#.#..##......###...###",
      "",
      "...## => #",
      "..#.. => #",
      ".#... => #",
      ".#.#. => #",
      ".#.## => #",
      ".##.. => #",
      ".#### => #",
      "#.#.# => #",
      "#.### => #",
      "##.#. => #",
      "##.## => #",
      "###.. => #",
      "###.# => #",
      "####. => #",
    )

    //when
    val solution = solvePart1(20)(lines)

    //then
    assert(solution == 325)
  }

}
