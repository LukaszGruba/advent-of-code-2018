package com.lukgru.algo.advent2018.day24

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object ImmuneSystemSimulator20XX {

  case class GroupUnit(hp: Int,
                       attack: Int,
                       attackType: String,
                       initiative: Int,
                       weaknesses: Set[String],
                       immunities: Set[String]
                      )

  case class Group(armyId: Int, groupId: Int, units: List[GroupUnit])

  case class Army(id: Int, groups: List[Group])

  def effectivePower(group: Group): Int =
    group.units.size * group.units.headOption.map(_.attack).getOrElse(0)

  def parseGroup(armyId: Int)(id: Int, groupStr: String): Group = {
    val sizeAndHpPattern = "(\\d+) units each with (\\d+) hit points.*".r
    val weaknessesPattern = ".*weak to ([a-z, ]*)[\\);].*".r
    val immunitiesPattern = ".*immune to ([a-z, ]*)[\\);].*".r
    val attackInitiativePattern = ".*with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+).*".r

    val sizeAndHpPattern(groupSize, hp) = groupStr
    val attackInitiativePattern(attack, attackType, initiative) = groupStr

    val weaknesses =
      if (groupStr.matches(weaknessesPattern.regex)) {
        val weaknessesPattern(weaknessesStr) = groupStr
        weaknessesStr.split(", ").toSet
      } else Set.empty[String]

    val immunities =
      if (groupStr.matches(immunitiesPattern.regex)) {
        val immunitiesPattern(immunitiesStr) = groupStr
        immunitiesStr.split(", ").toSet
      } else Set.empty[String]

    val modelUnit = GroupUnit(hp.toInt, attack.toInt, attackType, initiative.toInt, weaknesses, immunities)
    val units = (1 to groupSize.toInt).map(_ => modelUnit).toList
    Group(armyId, id, units)
  }

  def parseArmies(lines: List[String]): List[Army] = {
    def parseGroups(armyId: Int)(armyGroupStrs: List[(String, Int)]): List[Group] =
      armyGroupStrs.drop(1)
        .takeWhile(_._1 != "")
        .map { case (groupStr, id) => parseGroup(armyId)(id, groupStr) }

    val indexedLines = lines.zipWithIndex

    val immuneSystemGroups = parseGroups(0)(indexedLines.dropWhile(_._1 != "Immune System:"))
    val immuneSystemArmy = Army(0, immuneSystemGroups)

    val infectionGroups = parseGroups(1)(indexedLines.dropWhile(_._1 != "Infection:"))
    val infectionArmy = Army(1, infectionGroups)

    List(immuneSystemArmy, infectionArmy)
  }

  def dealDamage(attacker: Group, defender: Group): Option[Group] = {
    @tailrec
    def killTheWeakest(damageToDeal: Int, remainingUnits: List[GroupUnit]): List[GroupUnit] =
      remainingUnits match {
        case Nil => Nil
        case unit +: _ if damageToDeal <= 0 || unit.hp > damageToDeal => remainingUnits
        case unit +: rest => killTheWeakest(damageToDeal - unit.hp, rest)
      }

    calcTotalDamage(attacker, defender).flatMap { totalDamage =>
      val fromWeakestToStrongest = defender.units.sortBy(_.hp)
      val remainingUnits = killTheWeakest(totalDamage, fromWeakestToStrongest)
      remainingUnits match {
        case Nil => None
        case _ => Some(defender.copy(units = remainingUnits))
      }
    }
  }

  def calcTotalDamage(attacker: Group, defender: Group): Option[Int] = {
    val attackType = attacker.units.head.attackType
    if (defender.units.head.immunities.contains(attackType)) {
      None
    } else {
      val baseDamage = effectivePower(attacker)
      val damage = if (defender.units.head.weaknesses.contains(attackType)) 2 * baseDamage else baseDamage
      Some(damage)
    }
  }

  def targetSelectionPhase(armies: List[Army]): Map[Int, Int] = {
    val allGroups = armies.flatMap(_.groups)
    val sortedForTargetSelection = allGroups.sortBy(group => (effectivePower(group), group.units.head.initiative)).reverse

    @tailrec
    def selectTargets(queue: List[Group], availableTargets: List[Group], selections: Map[Int, Int]): Map[Int, Int] =
      queue match {
        case Nil => selections
        case _ if availableTargets.isEmpty => selections
        case selecting +: rest =>
          val selectedTarget = selectTarget(selecting, availableTargets)
          selectedTarget match {
            case None => selectTargets(rest, availableTargets, selections)
            case Some(target) =>
              val remainingTargets = availableTargets.filterNot(target.==)
              val updatedSelections = selections + (selecting.groupId -> target.groupId)
              selectTargets(rest, remainingTargets, updatedSelections)
          }
      }

    def selectTarget(selecting: Group, remainingTargets: List[Group]): Option[Group] = {
      val enemyGroups = remainingTargets.filter(_.armyId != selecting.armyId)
      enemyGroups.sortBy { enemyGroup =>
        (calcTotalDamage(selecting, enemyGroup), effectivePower(enemyGroup))
      }.lastOption
    }

    selectTargets(sortedForTargetSelection, allGroups, Map.empty)
  }

  @tailrec
  def attack(attackers: List[Group], targetSelections: Map[Int, Int], alreadyAttacked: List[Group]): List[Group] =
    attackers match {
      case Nil => alreadyAttacked
      case attacker +: rest =>
        val defenderIdOpt = targetSelections.get(attacker.groupId)
        val defender = defenderIdOpt.flatMap{ defenderId =>
          (attackers ++ alreadyAttacked).find(_.groupId == defenderId)
        }
        val defenderAfterAttackOpt = defender.flatMap(defenderOpt => dealDamage(attacker, defenderOpt))
        defenderAfterAttackOpt match {
          case None =>
            val filteredRest = rest.filterNot(g => defenderIdOpt.contains(g.groupId))
            val filteredAlreadyAttacked = (attacker +: alreadyAttacked).filterNot(g => defenderIdOpt.contains(g.groupId))
            attack(filteredRest, targetSelections, filteredAlreadyAttacked)
          case Some(defenderAfterAttack) =>
            val restAfterAttack =
              if (rest.contains(defender.get)) {
                val indexOfDefender = rest.indexOf(defender.get)
                rest.updated(indexOfDefender, defenderAfterAttack)
              } else rest
            val resultAfterAttack =
              if (alreadyAttacked.contains(defender.get)) {
                val indexOfDefender = alreadyAttacked.indexOf(defender.get)
                alreadyAttacked.updated(indexOfDefender, defenderAfterAttack)
              } else alreadyAttacked
            attack(restAfterAttack, targetSelections, attacker +: resultAfterAttack)
        }
    }

  def attackPhase(armies: List[Army], targetSelections: Map[Int, Int]): List[Army] = {
    def attackingOrder(groups: List[Group]): List[Group] = groups.sortBy(_.units.head.initiative).reverse

    val allGroups = armies.flatMap(_.groups)
    val groupsInAttackingOrder = attackingOrder(allGroups)

    val groupsAfterAttacks = attack(groupsInAttackingOrder, targetSelections, List.empty)
    val armiesAfterAttack = groupsAfterAttacks.groupBy(_.armyId).map { case (armyId, groups) => Army(armyId, groups) }
    armiesAfterAttack.toList
  }

  @tailrec
  def fight(armies: List[Army]): Army = {
    val attackerWithDefenderPairs = targetSelectionPhase(armies)
    val armiesAfterAttackPhase = attackPhase(armies, attackerWithDefenderPairs)
    armiesAfterAttackPhase match {
      case List(winner) => winner
      case _ => fight(armiesAfterAttackPhase)
    }
  }

  def solvePart1(lines: List[String]): Int = {
    val armies = parseArmies(lines)
    val winningArmy = fight(armies)
    winningArmy.groups.map(_.units.length).sum
  }

  def solvePart2(lines: List[String]): Int = {
    val armies = parseArmies(lines)

    @tailrec
    def iterUntilImmuneSystemWins(armies: List[Army], immuneSystemAttackBoost: Int): Army = {
      println(s"Boost value: $immuneSystemAttackBoost")
      val immuneSystemArmy = armies.find(a => a.id == 0).get
      val infectionArmy = armies.find(a => a.id == 1).get
      val boostedGroups = immuneSystemArmy.groups.map { group =>
        group.copy(units = group.units.map(u => u.copy(attack = u.attack + immuneSystemAttackBoost)))
      }
      val boostedImmuneSystemArmy = immuneSystemArmy.copy(groups = boostedGroups)
      val winningArmy = fight(List(boostedImmuneSystemArmy, infectionArmy))
      if (winningArmy.id == 0) winningArmy
      else iterUntilImmuneSystemWins(armies, immuneSystemAttackBoost + 1)
    }

    val solution = iterUntilImmuneSystemWins(armies, 0)
    solution.groups.map(_.units.length).sum
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day24-input")
    val solution1 = solvePart1(input)
    println(solution1)

    val solution2 = solvePart2(input)
    println(solution2)
  }

}
