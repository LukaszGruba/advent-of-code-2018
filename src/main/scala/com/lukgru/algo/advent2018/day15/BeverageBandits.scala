package com.lukgru.algo.advent2018.day15

import com.lukgru.algo.advent2018.day15.BeverageBandits.CreatureType.CreatureType
import com.lukgru.algo.advent2018.utils.InputLoader

object BeverageBandits {

  object CreatureType extends Enumeration {
    type CreatureType = Value
    val Elf, Goblin = Value
  }

  case class Position(x: Int, y: Int)

  case class Creature(pos: Position, cType: CreatureType, hp: Int = 200, attackPower: Int = 3)

  def parseCaveMap(lines: List[String]): Set[Position] =
    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (xs, y) =>
        xs.filter { case (c, _) => c != '#' }
          .map { case (_, x) => Position(x, y) }
      }.toSet

  def parseCreatures(lines: List[String]): List[Creature] = {
    def cType(c: Char): CreatureType = c match {
      case 'E' => CreatureType.Elf
      case 'G' => CreatureType.Goblin
    }

    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (xs, y) =>
        xs.filter { case (c, _) => c == 'G' || c == 'E' }
          .map { case (c, x) => Creature(Position(x, y), cType(c)) }
      }
  }

  def sortByPosition(creatures: List[Creature]): List[Creature] =
    creatures.sortBy { case Creature(Position(x, y), _, _, _) => (y, x) }

  def findEnemies(attackerType: CreatureType, allCreatures: List[Creature]): List[Creature] =
    allCreatures.filterNot(_.cType == attackerType)

  def findShortestPath(map: Set[Position])(start: Position, end: Position): List[Position] = ???

  def findPathToNearestEnemy(map: Set[Position])(creature: Creature, allCreatures: List[Creature]): (Position, List[Position]) = {
    val enemies = findEnemies(creature.cType, allCreatures)
    val allowedTerrain = map.filterNot(p => allCreatures.map(_.pos).exists(p.==))

    val paths = enemies.map(enemy => enemy -> findShortestPath(allowedTerrain)(creature.pos, enemy.pos))
    val (nearestEnemy, shortestPath) = paths.minBy { case (enemy, path) => (path.length, enemy.pos.y, enemy.pos.x) }
    (nearestEnemy.pos, shortestPath)
  }

  def moveStep(c: Creature): Creature = ???

  def attackNearestEnemy(attacker: Creature, allCreatures: List[Creature]): List[Creature] = ???

  def runSimulation(input: List[String]): (Int, Int, CreatureType) = ???

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day15-input")
    val (numberOfFullRounds, totalHPLeft, winningArmy) = runSimulation(input)
    println(s"Combat ends after $numberOfFullRounds rounds.\n" +
      s"${winningArmy}s win with $totalHPLeft total hit points left.\n" +
      s"Outcome: $numberOfFullRounds * $totalHPLeft = ${numberOfFullRounds * totalHPLeft}"
    )
  }

}
