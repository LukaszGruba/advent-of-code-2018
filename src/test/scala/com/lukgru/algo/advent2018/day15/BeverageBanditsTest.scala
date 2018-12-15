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
      p(1, 1), p(2, 1), p(3, 1), p(4, 1), p(5, 1),
      p(1, 2), p(2, 2), p(3, 2), p(4, 2), p(5, 2),
      p(1, 3), p(3, 3), p(5, 3),
      p(1, 4), p(2, 4), p(3, 4), p(5, 4),
      p(1, 5), p(2, 5), p(3, 5), p(4, 5), p(5, 5)
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
      Creature(p(2, 1), CreatureType.Goblin),
      Creature(p(4, 2), CreatureType.Elf),
      Creature(p(5, 2), CreatureType.Goblin),
      Creature(p(5, 3), CreatureType.Goblin),
      Creature(p(3, 4), CreatureType.Goblin),
      Creature(p(5, 4), CreatureType.Elf)
    )
    assert(creatures == expectedCreatures)
  }

  test("should sort creatures by position") {
    //given
    val creatures = List(
      Creature(p(5, 3), CreatureType.Goblin),
      Creature(p(2, 1), CreatureType.Goblin),
      Creature(p(5, 2), CreatureType.Goblin),
      Creature(p(3, 4), CreatureType.Goblin),
      Creature(p(5, 4), CreatureType.Elf),
      Creature(p(4, 2), CreatureType.Elf),
    )

    //when
    val sorted = BeverageBandits.sortByPosition(creatures)

    //then
    val expected = List(
      Creature(p(2, 1), CreatureType.Goblin),
      Creature(p(4, 2), CreatureType.Elf),
      Creature(p(5, 2), CreatureType.Goblin),
      Creature(p(5, 3), CreatureType.Goblin),
      Creature(p(3, 4), CreatureType.Goblin),
      Creature(p(5, 4), CreatureType.Elf)
    )
    assert(sorted == expected)
  }

  test("should find enemies") {
    //given
    val creatures = List(
      Creature(p(2, 1), CreatureType.Goblin),
      Creature(p(4, 2), CreatureType.Elf),
      Creature(p(5, 2), CreatureType.Goblin),
      Creature(p(5, 3), CreatureType.Goblin),
      Creature(p(3, 4), CreatureType.Goblin),
      Creature(p(5, 4), CreatureType.Elf)
    )

    //when
    val enemies = BeverageBandits.findEnemies(CreatureType.Elf, creatures)

    //then
    assert(enemies == List(
      Creature(p(2, 1), CreatureType.Goblin),
      Creature(p(5, 2), CreatureType.Goblin),
      Creature(p(5, 3), CreatureType.Goblin),
      Creature(p(3, 4), CreatureType.Goblin)
    ))
  }

  test("should find shortest path") {
    //given
    //given
    val input = List(
      "#######",
      "#S....#",
      "#.....#",
      "#.#.#.#",
      "##..#.#",
      "#E..#.#",
      "#######"
    )
    val map = BeverageBandits.parseCaveMap(input)
    val start = Position(1, 1)
    val end = Position(1, 5)

    //when
    val shortestPath = BeverageBandits.findShortestPath(map)(start, end)

    //then
    assert(shortestPath.isDefined)
    assert(shortestPath.get == List(p(1, 1), p(2, 1), p(3, 1), p(3, 2), p(3, 3), p(3, 4), p(2, 4), p(2, 5), p(1, 5)))
  }

  test("should locate nearest enemy and the shortest path to him") {
    //given
    val input = List(
      "#######",
      "#E....#",
      "#.....#",
      "#.E.#.#",
      "##..#.#",
      "#G..#G#",
      "#######"
    )
    val map = BeverageBandits.parseCaveMap(input)
    val allCreatures = BeverageBandits.parseCreatures(input)
    val elf = Creature(p(1, 1), CreatureType.Elf)

    //when
    val (nearestEnemyPosition, shortestPath) = BeverageBandits.findPathToNearestEnemy(map)(elf, allCreatures)

    //then
    assert(nearestEnemyPosition == p(1, 5))
    assert(shortestPath == List(p(1, 1), p(2, 1), p(3, 1), p(3, 2), p(3, 3), p(3, 4), p(2, 4), p(2, 5), p(1, 5)))
  }
}
