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
    val enemyPathOpt = BeverageBandits.findPathToNearestEnemy(map)(elf, allCreatures)

    //then
    assert(enemyPathOpt.isDefined)
    val (nearestEnemyPosition, shortestPath) = enemyPathOpt.get
    assert(nearestEnemyPosition == p(1, 5))
    assert(shortestPath == List(p(1, 1), p(2, 1), p(3, 1), p(3, 2), p(3, 3), p(3, 4), p(2, 4), p(2, 5), p(1, 5)))
  }

  test("should attack nearest enemy") {
    //given
    val creature = Creature(p(10, 10), CreatureType.Elf)
    val allCreatures = List(
      creature,
      Creature(p(9, 10), CreatureType.Elf),
      Creature(p(11, 10), CreatureType.Goblin),
      Creature(p(10, 9), CreatureType.Elf),
      Creature(p(10, 11), CreatureType.Goblin)
    )

    //when
    val newAllCreatures = BeverageBandits.attackNearestEnemy(creature, allCreatures)

    //then
    assert(newAllCreatures == List(
      creature,
      Creature(p(9, 10), CreatureType.Elf),
      Creature(p(11, 10), CreatureType.Goblin, hp = 197),
      Creature(p(10, 9), CreatureType.Elf),
      Creature(p(10, 11), CreatureType.Goblin)
    ))
  }

  test("should take several turns") {
    //given
    val input = List(
      "#########",
      "#E.E.E..#",
      "#...G...#",
      "#.G.E.G.#",
      "#.......#",
      "#G..G..G#",
      "#.......#",
      "#.......#",
      "#########"
    )
    val map = BeverageBandits.parseCaveMap(input)
    val initAll = BeverageBandits.parseCreatures(input).map(c => c.copy(hp = 1))
    val initC = initAll.head

    //when
    val (c1, all1) = BeverageBandits.takeTurn(map)(initC, initAll)
    val (c2, all2) = BeverageBandits.takeTurn(map)(c1, all1)
    val (c3, all3) = BeverageBandits.takeTurn(map)(c2, all2)
    val (c4, all4) = BeverageBandits.takeTurn(map)(c3, all3)
    val (c5, all5) = BeverageBandits.takeTurn(map)(c4, all4)
    val (c6, all6) = BeverageBandits.takeTurn(map)(c5, all5)
    val (c7, all7) = BeverageBandits.takeTurn(map)(c6, all6)
    val (c8, all8) = BeverageBandits.takeTurn(map)(c7, all7)
    val (c9, all9) = BeverageBandits.takeTurn(map)(c8, all8)

    //then
    assert(List(c1, c2, c3, c4, c5, c6, c7, c8, c9).map(_.pos)
      == List(p(2, 1), p(2, 2), p(3, 2), p(4, 2), p(5, 2), p(6, 2), p(7, 2), p(7, 3), p(7, 4)))

    assert(all1.contains(Creature(p(2, 3), CreatureType.Goblin, hp = 1)))
    assert(!all2.contains(Creature(p(2, 3), CreatureType.Goblin, hp = 1)))

    assert(all2.contains(Creature(p(4, 2), CreatureType.Goblin, hp = 1)))
    assert(!all3.contains(Creature(p(4, 2), CreatureType.Goblin, hp = 1)))

    assert(all5.contains(Creature(p(6, 3), CreatureType.Goblin, hp = 1)))
    assert(!all6.contains(Creature(p(6, 3), CreatureType.Goblin, hp = 1)))

    assert(all8.contains(Creature(p(7, 5), CreatureType.Goblin, hp = 1)))
    assert(!all9.contains(Creature(p(7, 5), CreatureType.Goblin, hp = 1)))
  }
}
