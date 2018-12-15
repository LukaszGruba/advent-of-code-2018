package com.lukgru.algo.advent2018.day15

import com.lukgru.algo.advent2018.day15.BeverageBandits.{Creature, CreatureType, Position}
import org.scalatest.FunSuite

class BeverageBanditsTest extends FunSuite {

  def p = Position

  test("should parse cave map - allowed positions in the cave") {
    //given
    val map = List(
      "#######",
      "#.G...#",
      "#...EG#",
      "#.#.#G#",
      "#..G#E#",
      "#.....#",
      "#######",
    )

    //when
    val caveMap = BeverageBandits.parseCaveMap(map)

    //then
    val expectedCaveMap = Set(
      p(1,1), p(2,1), p(3,1), p(4,1), p(5,1),
      p(1,2), p(2,2), p(3,2), p(4,2), p(5,2),
      p(1,3), p(3,3), p(5,3),
      p(1,4), p(2,4), p(3,4), p(5,4),
      p(1,5), p(2,5), p(3,5), p(4,5), p(5,5)
    )
    expectedCaveMap.foreach(expectedPos => assert(caveMap.contains(expectedPos)))
    assert(caveMap == expectedCaveMap)
  }

  test("should parse creatures") {
    //given
    val map = List(
      "#######",
      "#.G...#",
      "#...EG#",
      "#.#.#G#",
      "#..G#E#",
      "#.....#",
      "#######",
    )

    //when
    val creatures = BeverageBandits.parseCreatures(map)

    //then
    val expectedCreatures = List(
      Creature(p(2,1), CreatureType.Goblin),
      Creature(p(4,2), CreatureType.Elf),
      Creature(p(5,2), CreatureType.Goblin),
      Creature(p(5,3), CreatureType.Goblin),
      Creature(p(3,4), CreatureType.Goblin),
      Creature(p(5,4), CreatureType.Elf)
    )
    assert(creatures == expectedCreatures)
  }
}
