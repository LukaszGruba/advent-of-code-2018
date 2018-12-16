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

  def nearestPositions(pos: Position): List[Position] =
    List(Position(pos.x, pos.y - 1), Position(pos.x - 1, pos.y), Position(pos.x + 1, pos.y), Position(pos.x, pos.y + 1))

  def findShortestPath(map: Set[Position])(start: Position, end: Position): Option[List[Position]] = {
    def possibleNextSteps(pos: Position, available: Set[Position]): List[Position] =
      nearestPositions(pos).filter(available.contains)

    def bfs(current: Position, target: Position, available: Set[Position], path: List[Position] = List.empty): Option[List[Position]] = {
      if (nearestPositions(current).contains(target)) Some(path :+ current :+ target)
      else if (available.isEmpty) None
      else {
        val possiblePaths = possibleNextSteps(current, available).flatMap { pos =>
          bfs(pos, target, available.filterNot(current.==))
        }
        possiblePaths match {
          case Nil => None
          case paths => Some(current +: paths.minBy(_.length))
        }
      }
    }

    bfs(start, end, map)
  }

  def findPathToNearestEnemy(map: Set[Position])(creature: Creature, allCreatures: List[Creature]): Option[(Position, List[Position])] = {
    val enemies = findEnemies(creature.cType, allCreatures)
    val allowedTerrain = map.filterNot(p => allCreatures.map(_.pos).exists(p.==))

    val paths = enemies.map(enemy => enemy -> findShortestPath(allowedTerrain)(creature.pos, enemy.pos))
      .filter { case (enemy, pathOpt) => pathOpt.isDefined }
      .map { case (enemy, pathOpt) => (enemy, pathOpt.get) }
    paths match {
      case Nil => None
      case _ =>
        val (nearestEnemy, shortestPath) = paths.minBy { case (enemy, path) => (path.length, enemy.pos.y, enemy.pos.x) }
        Some(nearestEnemy.pos, shortestPath)
    }
  }

  def move(creature: Creature, path: List[Position]): Creature = {
    if (path.length <= 2) creature
    else creature.copy(pos = path(1))
  }

  def attackNearestEnemy(attacker: Creature, allCreatures: List[Creature]): List[Creature] = {
    val nearestEnemy = findEnemies(attacker.cType, allCreatures).find(c => nearestPositions(attacker.pos).contains(c.pos))
    nearestEnemy match {
      case None => allCreatures
      case Some(enemy) => allCreatures.map { c =>
        if (c == enemy) c.copy(hp = c.hp - attacker.attackPower)
        else c
      }.filterNot(_.hp <= 0)
    }
  }

  def takeTurn(map: Set[Position])(creature: Creature, allCreatures: List[Creature]): (Creature, List[Creature]) = {
    findPathToNearestEnemy(map)(creature, allCreatures) match {
      case None => (creature, allCreatures)
      case Some((_, path)) =>
        val creatureAfterMove = move(creature, path)
        val allOtherCreatures = allCreatures.filterNot(creature.==)
        val creaturesAfterAttack = attackNearestEnemy(creatureAfterMove, allOtherCreatures)
        (creatureAfterMove, creaturesAfterAttack :+ creatureAfterMove)
    }
  }

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
