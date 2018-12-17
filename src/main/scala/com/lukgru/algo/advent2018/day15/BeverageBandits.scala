package com.lukgru.algo.advent2018.day15

import com.lukgru.algo.advent2018.day15.BeverageBandits.CreatureType.CreatureType
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.collection.mutable

object BeverageBandits {

  var printingOn = true

  object CreatureType extends Enumeration {
    type CreatureType = Value
    val Elf, Goblin = Value
  }

  case class Position(x: Int, y: Int) {
    override def toString: String = s"p($x,$y)"
  }

  case class Creature(id: String, pos: Position, cType: CreatureType, hp: Int = 200, attackPower: Int = 3) {
    override def toString: String = s"$cType($id, (${pos.x},${pos.y}), $hp, $attackPower)"
  }

  def parseCaveMap(lines: List[String]): Set[Position] =
    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (xs, y) =>
        xs.filter { case (c, _) => c != '#' }
          .map { case (_, x) => Position(x, y) }
      }.toSet

  def parseCreatures(lines: List[String], elvesPower: Int = 3): List[Creature] = {
    def cType(c: Char): CreatureType = c match {
      case 'E' => CreatureType.Elf
      case 'G' => CreatureType.Goblin
    }

    def power(creatureType: CreatureType): Int = creatureType match {
      case CreatureType.Goblin => 3
      case CreatureType.Elf => elvesPower
    }

    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (xs, y) =>
        xs.filter { case (c, _) => c == 'G' || c == 'E' }
          .map { case (c, x) =>
            val race = cType(c)
            Creature(s"$x $y", Position(x, y), race, attackPower = power(race)) }
      }
  }

  def sortByPosition(creatures: List[Creature]): List[Creature] =
    creatures.sortBy { case Creature(_, Position(x, y), _, _, _) => (y, x) }

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
        val (nearestEnemy, shortestPath) = paths.minBy { case (enemy, path) => (path.length, path(path.length - 2).y, path(path.length - 2).x) }
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

  def takeTurn(map: Set[Position])(creature: Creature, allCreatures: List[Creature]): (Creature, List[Creature]) =
    findPathToNearestEnemy(map)(creature, allCreatures) match {
      case None => (creature, allCreatures)
      case Some((_, path)) =>
        val creatureAfterMove = move(creature, path)
        val allOtherCreatures = allCreatures.filterNot(c => c.id == creatureAfterMove.id)
        val creaturesAfterAttack = attackNearestEnemy(creatureAfterMove, allOtherCreatures)
        (creatureAfterMove, creaturesAfterAttack :+ creatureAfterMove)
    }

  def playRound(map: Set[Position])(allCreatures: List[Creature]): (List[Creature], Boolean) = {
    val sorted = sortByPosition(allCreatures)
    val (afterRound, lastTurnEndedWithKill) = sorted.foldLeft((sorted, false)) {
      case ((creatures, lastOneKilled), currentCreature) =>
        creatures.find(_.id == currentCreature.id)
          .map { attacker =>
            val creaturesAfterTurn = takeTurn(map)(attacker, creatures)._2
            val didIKill = creaturesAfterTurn.length < creatures.length
            (creaturesAfterTurn, didIKill)
          }
          .getOrElse((creatures, lastOneKilled))
    }
    (sortByPosition(afterRound), lastTurnEndedWithKill)
  }

  def isWarOver(allCreatures: List[Creature]): Boolean = allCreatures.groupBy(_.cType).size == 1

  def playWar(map: Set[Position])(creatures: List[Creature]): (Int, Int, CreatureType, List[Creature]) = {
    var currentCreatures = creatures
    var roundNo = 0
    var lastRoundEndedWithKill = false
    while (!isWarOver(currentCreatures)) {
      val (afterRound, lastTurnWithKill) = playRound(map)(currentCreatures)
      currentCreatures = afterRound
      lastRoundEndedWithKill = lastTurnWithKill
      roundNo += 1
      printState(map)(currentCreatures)
    }
    val sumOfHp = currentCreatures.map(_.hp).sum
    val winnerRace = currentCreatures.head.cType
    val creaturesLeft = currentCreatures
    val numberOfFullRounds = if (lastRoundEndedWithKill) roundNo else roundNo - 1
    (numberOfFullRounds, sumOfHp, winnerRace, creaturesLeft)
  }

  def runSimulation(input: List[String], elvesPower: Int = 3): (Int, Int, CreatureType, List[Creature]) = {
    val map = parseCaveMap(input)
    val creatures = sortByPosition(parseCreatures(input, elvesPower))
    playWar(map)(creatures)
  }

  def printState(map: Set[Position])(creatures: List[Creature]): Unit =
    if (printingOn) {
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

  def printOutcome(numberOfFullRounds: Int, totalHPLeft: Int, winningArmy: CreatureType): Unit =
    println(s"Combat ends after $numberOfFullRounds rounds.\n" +
      s"${winningArmy}s win with $totalHPLeft total hit points left.\n" +
      s"Outcome: $numberOfFullRounds * $totalHPLeft = ${numberOfFullRounds * totalHPLeft}"
    )

  def simulateNoElfCasualties(input: List[String]): (Int, Int, CreatureType, Int) = {
    def countElves(creatures: List[Creature]): Int = creatures.count(_.cType == CreatureType.Elf)
    val initElvesCount = countElves(parseCreatures(input))
    Stream.from(0)
      .map(elvesPower => (elvesPower, runSimulation(input, elvesPower)))
      .find { case (_, (_, _, _, winningArmyState)) => countElves(winningArmyState) == initElvesCount }
      .map { case (power, (numberOfRounds, totalHP, winningArmy, _)) => (numberOfRounds, totalHP, winningArmy, power)}
      .get
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day15-input")
    val (numberOfFullRounds, totalHPLeft, winningArmy, _) = runSimulation(input)
    printOutcome(numberOfFullRounds, totalHPLeft, winningArmy)

    val (numberOfFullRounds2, totalHPLeft2, winningArmy2, elvesPower) = simulateNoElfCasualties(input)
    printOutcome(numberOfFullRounds2, totalHPLeft2, winningArmy2)
    println(s"${winningArmy2}s should have at least $elvesPower power to win without any casualties")
  }

}
