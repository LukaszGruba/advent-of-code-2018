package com.lukgru.algo.advent2018.day15

import com.lukgru.algo.advent2018.day15.BeverageBandits.CreatureType.CreatureType
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.collection.mutable

object BeverageBandits {

  object CreatureType extends Enumeration {
    type CreatureType = Value
    val Elf, Goblin = Value
  }

  case class Position(x: Int, y: Int) {
    override def toString: String = s"p($x,$y)"
  }

  case class Creature(pos: Position, cType: CreatureType, hp: Int = 200, attackPower: Int = 3) {
    override def toString: String = s"$cType((${pos.x},${pos.y}), $hp)"
  }

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

    def bfsImper(start: Position, end: Position): Option[List[Position]] = {
      def possibleNextSteps(pos: Position, available: mutable.Set[Position]): List[Position] =
        nearestPositions(pos).filter(available.contains)

      val prevs = mutable.Map.empty[Position, Position]
      val notVisited = mutable.Set() ++ map + start + end
      var remaining = mutable.Queue(start)
      var current = start
      while (current != end && remaining.nonEmpty) {
        current = remaining.dequeue()
        possibleNextSteps(current, notVisited).foreach {
          possibleStep =>
            if (!remaining.contains(possibleStep)) {
              remaining += possibleStep
            }
            if (!prevs.contains(possibleStep)) {
              prevs.put(possibleStep, current)
            }
        }
        notVisited.remove(current)
      }
      if (current == end) {
        var c = end
        var path = List(c)
        while (c != start) {
          val prev = prevs(c)
          path = prev +: path
          c = prev
        }
        Some(path)
      }
      else None
    }

    def bfs2(s: Stream[Position], f: Position => Stream[Position]): Stream[Position] = {
      if (s.isEmpty) s
      else s.head #:: bfs2(s.tail.append(f(s.head)), f)
    }

    //    bfs(start, end, map)
    bfsImper(start, end)
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
    val nearestWeakestEnemy = findEnemies(attacker.cType, allCreatures)
      .filter(c => nearestPositions(attacker.pos).contains(c.pos))
      .sortBy(enemy => (enemy.hp, enemy.pos.y, enemy.pos.x))
      .headOption
    nearestWeakestEnemy match {
      case None => allCreatures
      case Some(enemy) => allCreatures.map { c =>
        if (c == enemy) c.copy(hp = c.hp - attacker.attackPower)
        else c
      }.filterNot(_.hp <= 0)
    }
  }

  def takeTurn(map: Set[Position])(creature: Creature, allCreatures: List[Creature]): (Creature, List[Creature]) = {
    if (isWarOver(allCreatures)) {
      println("WAR IS OVER!")
    }
    findPathToNearestEnemy(map)(creature, allCreatures) match {
      case None => (creature, allCreatures)
      case Some((_, path)) =>
        val creatureAfterMove = move(creature, path)
        val allOtherCreatures = allCreatures.filterNot(c => c.pos == creature.pos)
        val creaturesAfterAttack = attackNearestEnemy(creatureAfterMove, allOtherCreatures)
        (creatureAfterMove, creaturesAfterAttack :+ creatureAfterMove)
    }
  }

  def playRound(map: Set[Position])(allCreatures: List[Creature]): List[Creature] = {
    val sorted = sortByPosition(allCreatures)
    val afterRound = sorted.foldLeft(sorted) {
      case (creatures, currentCreature) =>
        creatures.find(_.pos == currentCreature.pos)
          .map(takeTurn(map)(_, creatures)._2)
          .getOrElse(creatures)
    }
    sortByPosition(afterRound)
  }

  def isWarOver(allCreatures: List[Creature]): Boolean = allCreatures.groupBy(_.cType).size == 1

  def playWar(map: Set[Position])(creatures: List[Creature]): (Int, Int, CreatureType) = {
    var currentCreatures = creatures
    var roundNo = 0
    println("INIT STATE:")
    printState(map)(currentCreatures)
    while (!isWarOver(currentCreatures)) {
      currentCreatures = playRound(map)(currentCreatures)
      roundNo += 1
      println(roundNo)
      printState(map)(currentCreatures)
    }
    println("FINAL STATE:")
    printState(map)(currentCreatures)
    val sumOfHp = currentCreatures.map(_.hp).sum
    val winnerRace = currentCreatures.head.cType
    (roundNo - 1, sumOfHp, winnerRace)
  }

  def runSimulation(input: List[String]): (Int, Int, CreatureType) = {
    val map = parseCaveMap(input)
    val creatures = sortByPosition(parseCreatures(input))
    playWar(map)(creatures)
  }

  def printState(map: Set[Position])(creatures: List[Creature]): Unit = {
    val width = map.map(_.x).max
    val height = map.map(_.y).max
    val sb = new StringBuilder()
    sb.append(creatures).append('\n')
    for (y <- 0 to height + 1) {
      for (x <- 0 to width + 1) {
        val p = Position(x, y)
        val c =
          if (creatures.exists(_.pos == p)) creatures.find(_.pos == p).get.cType.toString.head
          else if (map.contains(p)) '.'
          else '#'
        sb.append(c)
      }
      sb.append("   ")
      creatures.filter(_.pos.y == y).sortBy(_.pos.x).foreach(sb.append(_).append('\t'))
      sb.append('\n')
    }
    println(sb.mkString)
  }

  //75 * 2659 = 199425 -> TOO LOW
  //            200500 -> NOT RIGHT
  //76 * 2659 = 202084 -> TOO HIGH
  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day15-input")
    val (numberOfFullRounds, totalHPLeft, winningArmy) = runSimulation(input)
    println(s"Combat ends after $numberOfFullRounds rounds.\n" +
      s"${winningArmy}s win with $totalHPLeft total hit points left.\n" +
      s"Outcome: $numberOfFullRounds * $totalHPLeft = ${numberOfFullRounds * totalHPLeft}"
    )
  }

}
