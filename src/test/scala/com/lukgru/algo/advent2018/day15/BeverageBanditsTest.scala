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

  test("should find shortest path for big map") {
    //given
    val input = List(
      "################################",
      "#########################.S.####",
      "#########################....###",
      "##################.#.........###",
      "##################.##.......####",
      "#################...#.........##",
      "################..............##",
      "######..########...#...#.#....##",
      "#####....######.#.##.....##.####",
      "#######.######............#.####",
      "#####............#......#...####",
      "#####..#......................##",
      "########......#####..........###",
      "#######......#######..........##",
      "######...#.##########........###",
      "######......#########........###",
      "#####.......#########........###",
      "#####....#..#########........###",
      "######.##.#.#########......#####",
      "#######......#######.......#####",
      "#######.......#####....#...#####",
      "##.#..#.##............##.....###",
      "#.....#........###..#.#.....####",
      "#.........#.#...#####.#.#....###",
      "######......#.....###...#.#.####",
      "#####........##...###..####..###",
      "####...##.##....######.####...##",
      "####.#########....####.####....#",
      "###...#######.....####.####....#",
      "####..#######.##.##########...##",
      "####..######################.###",
      "################################"
    )
    val map = BeverageBandits.parseCaveMap(input)
    val start = Position(26, 1)
    val end = Position(5, 30)

    //when
    val shortestPath = BeverageBandits.findShortestPath(map)(start, end)

    //then
    assert(shortestPath.isDefined)
    assert(shortestPath.get == List(
      p(26, 1), p(25, 1),
      p(25, 2),
      p(25, 3), p(24, 3), p(23, 3), p(22, 3), p(21, 3),
      p(21, 4),
      p(21, 5),
      p(21, 6),
      p(21, 7),
      p(21, 8),
      p(21, 9),
      p(21, 10),
      p(21, 11),
      p(21, 12),
      p(21, 13),
      p(21, 14),
      p(21, 15),
      p(21, 16),
      p(21, 17),
      p(21, 18),
      p(21, 19), p(20, 19),
      p(20, 20), p(19, 20),
      p(19, 21), p(18, 21), p(17, 21), p(16, 21), p(15, 21), p(14, 21), p(13, 21), p(12, 21), p(11, 21), p(10, 21),
      p(10, 22), p(9, 22), p(8, 22), p(7, 22),
      p(7, 23), p(6, 23),
      p(6, 24),
      p(6, 25), p(5, 25),
      p(5, 26), p(4, 26),
      p(4, 27),
      p(4, 28), p(5, 28),
      p(5, 29),
      p(5, 30)
    ))
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

  test("should play several rounds") {
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
    val initAll = BeverageBandits.parseCreatures(input).map(c => c.copy(hp = 6))

    //when
    val all1 = BeverageBandits.playRound(map)(initAll)
    val all2 = BeverageBandits.playRound(map)(all1)
    val all3 = BeverageBandits.playRound(map)(all2)
    val all4 = BeverageBandits.playRound(map)(all3)
    val all5 = BeverageBandits.playRound(map)(all4)

    BeverageBandits.printState(map)(initAll)
    BeverageBandits.printState(map)(all1)
    BeverageBandits.printState(map)(all2)
    BeverageBandits.printState(map)(all3)
    BeverageBandits.printState(map)(all4)
    BeverageBandits.printState(map)(all5)

    //then
    //    #########
    //    #.E.E...# 3 6
    //    #.G..E..# 6  6
    //    #....EG.#    33
    //    #G..G..G#6  6  6
    //    #.......#
    //    #.......#
    //    #.......#
    //    #########
    assert(all1 == List(
      Creature(p(2, 1), CreatureType.Elf, hp = 3),
      Creature(p(4, 1), CreatureType.Elf, hp = 6),
      Creature(p(2, 2), CreatureType.Goblin, hp = 6),
      Creature(p(5, 2), CreatureType.Elf, hp = 6),
      Creature(p(5, 3), CreatureType.Elf, hp = 3),
      Creature(p(6, 3), CreatureType.Goblin, hp = 3),
      Creature(p(1, 4), CreatureType.Goblin, hp = 6),
      Creature(p(4, 4), CreatureType.Goblin, hp = 6),
      Creature(p(7, 4), CreatureType.Goblin, hp = 6)
    ))

    //    #########
    //    #..E....#  6
    //    #.G...E.# 3   6
    //    #G.....G#6     6
    //    #...G...#   3
    //    #.......#
    //    #.......#
    //    #.......#
    //    #########
    assert(all2 == List(
      Creature(p(3, 1), CreatureType.Elf, hp = 6),
      Creature(p(2, 2), CreatureType.Goblin, hp = 3),
      Creature(p(6, 2), CreatureType.Elf, hp = 6),
      Creature(p(1, 3), CreatureType.Goblin, hp = 6),
      Creature(p(7, 3), CreatureType.Goblin, hp = 6),
      Creature(p(4, 4), CreatureType.Goblin, hp = 3)
    ))

    //    #########
    //    #.E.....#  6
    //    #G.....E#6     3
    //    #...G..G#   3  3
    //    #.......#
    //    #.......#
    //    #.......#
    //    #.......#
    //    #########
    assert(all3 == List(
      Creature(p(2, 1), CreatureType.Elf, hp = 6),
      Creature(p(1, 2), CreatureType.Goblin, hp = 6),
      Creature(p(7, 2), CreatureType.Elf, hp = 3),
      Creature(p(4, 3), CreatureType.Goblin, hp = 3),
      Creature(p(7, 3), CreatureType.Goblin, hp = 3)
    ))

    //    #########
    //    #E......#3
    //    #G..G..E#3  3  3
    //    #.......#
    //    #.......#
    //    #.......#
    //    #.......#
    //    #.......#
    //    #########
    assert(all4 == List(
      Creature(p(1, 1), CreatureType.Elf, hp = 3),
      Creature(p(1, 2), CreatureType.Goblin, hp = 3),
      Creature(p(4, 2), CreatureType.Goblin, hp = 3),
      Creature(p(7, 2), CreatureType.Elf, hp = 3)
    ))

    //    #########
    //    #E......#3
    //    #.....E.#     3
    //    #.......#
    //    #.......#
    //    #.......#
    //    #.......#
    //    #.......#
    //    #########
    assert(all5 == List(
      Creature(p(1, 1), CreatureType.Elf, hp = 3),
      Creature(p(6, 2), CreatureType.Elf, hp = 3)
    ))
  }

  test("should play war for simple example") {
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
    val initAll = BeverageBandits.parseCreatures(input).map(c => c.copy(hp = 6))

    //when
    val (numberOfRounds, totalHP, winner) = BeverageBandits.playWar(map)(initAll)

    //then
    assert(numberOfRounds == 4)
    assert(totalHP == 6)
    assert(winner == CreatureType.Elf)

  }

  test("should run simulation for example1") {
    //given
    val input = List(
      "#######",
      "#G..#E#",
      "#E#E.E#",
      "#G.##.#",
      "#...#E#",
      "#...E.#",
      "#######"
    )

    //when
    val (numberOfRounds, totalHP, winner) = BeverageBandits.runSimulation(input)

    //then
    assert(numberOfRounds == 37)
    assert(totalHP == 982)
    assert(winner == CreatureType.Elf)
  }

  test("should run simulation for example2") {
    //given
    val input = List(
      "#######",
      "#E..EG#",
      "#.#G.E#",
      "#E.##E#",
      "#G..#.#",
      "#..E#.#",
      "#######"
    )

    //when
    val (numberOfRounds, totalHP, winner) = BeverageBandits.runSimulation(input)

    //then
    assert(numberOfRounds == 46)
    assert(totalHP == 859)
    assert(winner == CreatureType.Elf)
  }

  test("should run simulation for example3") {
    //given
    val input = List(
      "#######",
      "#E.G#.#",
      "#.#G..#",
      "#G.#.G#",
      "#G..#.#",
      "#...E.#",
      "#######"
    )

    //when
    val (numberOfRounds, totalHP, winner) = BeverageBandits.runSimulation(input)

    //then
    assert(numberOfRounds == 35)
    assert(totalHP == 793)
    assert(winner == CreatureType.Goblin)
  }

  test("should run simulation for example4") {
    //given
    val input = List(
      "#######",
      "#.E...#",
      "#.#..G#",
      "#.###.#",
      "#E#G#G#",
      "#...#G#",
      "#######"
    )

    //when
    val (numberOfRounds, totalHP, winner) = BeverageBandits.runSimulation(input)

    //then
    assert(numberOfRounds == 54)
    assert(totalHP == 536)
    assert(winner == CreatureType.Goblin)
  }

  test("should run simulation for example5") {
    //given
    val input = List(
      "#########",
      "#G......#",
      "#.E.#...#",
      "#..##..G#",
      "#...##..#",
      "#...#...#",
      "#.G...G.#",
      "#.....G.#",
      "#########"
    )

    //when
    val (numberOfRounds, totalHP, winner) = BeverageBandits.runSimulation(input)

    //then
    assert(numberOfRounds == 20)
    assert(totalHP == 937)
    assert(winner == CreatureType.Goblin)
  }

  test("should attack enemy with fewest HP first") {
    //given
    val attacker = Creature(Position(5,5), CreatureType.Elf)
    val all = List(
      Creature(Position(5,4), CreatureType.Goblin, hp = 10),
      Creature(Position(4,5), CreatureType.Goblin, hp = 9),
      attacker,
      Creature(Position(5,6), CreatureType.Goblin, hp = 1),
      Creature(Position(6,5), CreatureType.Goblin, hp = 2)
    )

    //when
    val afterAttack = BeverageBandits.attackNearestEnemy(attacker, all)

    //then
    val expected = List(
      Creature(Position(5,4), CreatureType.Goblin, hp = 10),
      Creature(Position(4,5), CreatureType.Goblin, hp = 9),
      attacker,
      Creature(Position(6,5), CreatureType.Goblin, hp = 2)
    )
    assert(afterAttack == expected)
  }

  test("should attack enemy with fewest HP first, if same HP -> in reading order") {
    //given
    val attacker = Creature(Position(5,5), CreatureType.Elf)
    val all = List(
      Creature(Position(5,4), CreatureType.Goblin, hp = 10),
      Creature(Position(4,5), CreatureType.Goblin, hp = 1),
      attacker,
      Creature(Position(5,6), CreatureType.Goblin, hp = 1),
      Creature(Position(6,5), CreatureType.Goblin, hp = 2)
    )

    //when
    val afterAttack = BeverageBandits.attackNearestEnemy(attacker, all)

    //then
    val expected = List(
      Creature(Position(5,4), CreatureType.Goblin, hp = 10),
      attacker,
      Creature(Position(5,6), CreatureType.Goblin, hp = 1),
      Creature(Position(6,5), CreatureType.Goblin, hp = 2)
    )
    assert(afterAttack == expected)
  }

}
