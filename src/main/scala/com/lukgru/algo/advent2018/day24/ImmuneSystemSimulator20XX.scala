package com.lukgru.algo.advent2018.day24

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object ImmuneSystemSimulator20XX {

  val printingEnabled: Boolean = false

  case class GroupUnit(hp: Int,
                       attack: Int,
                       attackType: String,
                       initiative: Int,
                       weaknesses: Set[String],
                       immunities: Set[String]
                      )

  case class Group(armyName: String, groupId: Int, units: List[GroupUnit])

  case class Army(name: String, groups: List[Group])

  def effectivePower(group: Group): Int =
    group.units.size * group.units.head.attack

  def parseGroup(armyName: String)(id: Int, groupStr: String): Group = {
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
    Group(armyName, id, units)
  }

  def parseArmies(lines: List[String]): List[Army] = {
    def parseGroups(armyName: String)(armyGroupStrs: List[(String, Int)]): List[Group] =
      armyGroupStrs.drop(1)
        .takeWhile(_._1 != "")
        .map { case (groupStr, id) => parseGroup(armyName)(id, groupStr) }

    val indexedLines = lines.zipWithIndex

    val immuneSystemGroups = parseGroups("Immune")(indexedLines.dropWhile(_._1 != "Immune System:"))
    val immuneSystemArmy = Army("Immune", immuneSystemGroups)

    val infectionGroups = parseGroups("Infection")(indexedLines.dropWhile(_._1 != "Infection:"))
    val infectionArmy = Army("Infection", infectionGroups)

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
      if (printingEnabled) println(s"\t${attacker.armyName}.${attacker.groupId} attacks ${defender.armyName}.${defender.groupId} for $totalDamage:\t\t${defender.units.length - remainingUnits.length} killed of ${defender.units.length}")
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
    def sortForTargetSelection(groups: List[Group]): List[Group] =
      groups.sortBy(group => (effectivePower(group), group.units.head.initiative)).reverse

    val allGroups = armies.flatMap(_.groups)
    val sortedForTargetSelection = sortForTargetSelection(allGroups)

    @tailrec
    def selectTargets(queue: List[Group], availableTargets: List[Group], selections: Map[Int, Int]): Map[Int, Int] =
      queue match {
        case Nil => selections
        case _ if availableTargets.isEmpty => selections
        case selecting +: rest =>
          val selectedTarget = selectTarget(selecting, availableTargets)
          selectedTarget match {
            case None => selectTargets(sortForTargetSelection(rest), availableTargets, selections)
            case Some(target) =>
              val remainingTargets = availableTargets.filterNot(t => t.groupId == target.groupId)
              val updatedSelections = selections + (selecting.groupId -> target.groupId)
              selectTargets(sortForTargetSelection(rest), remainingTargets, updatedSelections)
          }
      }

    def selectTarget(selecting: Group, remainingTargets: List[Group]): Option[Group] = {
      val enemyGroups = remainingTargets.filter(_.armyName != selecting.armyName)
      enemyGroups
        .filterNot(enemyGroup => calcTotalDamage(selecting, enemyGroup).getOrElse(0) == 0)
        .sortBy { enemyGroup =>
          (calcTotalDamage(selecting, enemyGroup).getOrElse(0), effectivePower(enemyGroup), enemyGroup.units.head.initiative)
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
        val defender = defenderIdOpt.flatMap { defenderId =>
          (attackers ++ alreadyAttacked).find(_.groupId == defenderId)
        }
        val defenderAfterAttackOpt = defender.flatMap(defenderOpt => dealDamage(attacker, defenderOpt))
        defenderAfterAttackOpt match {
          case None =>
            val filteredRest = rest.filterNot(g => defenderIdOpt.contains(g.groupId))
            val filteredAlreadyAttacked = (attacker +: alreadyAttacked).filterNot(g => defenderIdOpt.contains(g.groupId))
            attack(attackingOrder(filteredRest), targetSelections, filteredAlreadyAttacked)
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
            attack(attackingOrder(restAfterAttack), targetSelections, attacker +: resultAfterAttack)
        }
    }

  def attackingOrder(groups: List[Group]): List[Group] = groups.sortBy(_.units.head.initiative).reverse

  def attackPhase(armies: List[Army], targetSelections: Map[Int, Int]): List[Army] = {
    val allGroups = armies.flatMap(_.groups)
    val groupsInAttackingOrder = attackingOrder(allGroups)

    val groupsAfterAttacks = attack(groupsInAttackingOrder, targetSelections, List.empty)
    val armiesAfterAttack = groupsAfterAttacks.groupBy(_.armyName).map { case (armyName, groups) => Army(armyName, groups) }
    armiesAfterAttack.toList
  }

  @tailrec
  def fight(roundNo: Int, armies: List[Army]): Option[Army] = {
    if (printingEnabled) println(s"Round: $roundNo")
    val attackerWithDefenderPairs = targetSelectionPhase(armies)
    val armiesAfterAttackPhase = attackPhase(armies, attackerWithDefenderPairs)
    armiesAfterAttackPhase match {
      case `armies` =>
        println("STALEMATE!")
        None
      case List(winner) => Some(winner)
      case _ => fight(roundNo + 1, armiesAfterAttackPhase)
    }
  }

  def solvePart1(lines: List[String]): Int = {
    val armies = parseArmies(lines)
    val winningArmy = fight(1, armies)
    winningArmy.map { winner =>
      winner.groups.map(_.units.length).sum
    }.get
  }

  def solvePart2(lines: List[String]): Int = {
    val armies = parseArmies(lines)

    @tailrec
    def iterUntilImmuneSystemWins(armies: List[Army], immuneSystemAttackBoost: Int): Army = {
      println(s"Boost value: $immuneSystemAttackBoost")
      val immuneSystemArmy = armies.find(a => a.name == "Immune").get
      val infectionArmy = armies.find(a => a.name == "Infection").get
      val boostedGroups = immuneSystemArmy.groups.map { group =>
        group.copy(units = group.units.map(u => u.copy(attack = u.attack + immuneSystemAttackBoost)))
      }
      val boostedImmuneSystemArmy = immuneSystemArmy.copy(groups = boostedGroups)
      val winningArmy = fight(1, List(boostedImmuneSystemArmy, infectionArmy))
      winningArmy match {
        case None =>
          if (printingEnabled) println("STALEMATE!")
          iterUntilImmuneSystemWins(armies, immuneSystemAttackBoost + 1)
        case Some(winner) =>
          if (printingEnabled) {
            println(s"Winning army id: ${winner.name}")
            println(s"Winning army groups size: ${winner.groups.length}")
            println(s"Total units left: ${winner.groups.map(_.units.length).sum}\n")
          }
          if (winner.name == "Immune") winner
          else iterUntilImmuneSystemWins(armies, immuneSystemAttackBoost + 1)
      }
    }

    val winningArmy = iterUntilImmuneSystemWins(armies, 0)
    winningArmy.groups.map(_.units.length).sum
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day24-input")
    val solution1 = solvePart1(input)
    println(solution1)

    val solution2 = solvePart2(input)
    println(solution2)
  }

}
