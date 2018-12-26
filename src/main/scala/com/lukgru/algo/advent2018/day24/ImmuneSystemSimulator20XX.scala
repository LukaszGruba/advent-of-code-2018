package com.lukgru.algo.advent2018.day24

import com.lukgru.algo.advent2018.utils.InputLoader

object ImmuneSystemSimulator20XX {

  case class GroupUnit(hp: Int,
                       attack: Int,
                       attackType: String,
                       initiative: Int,
                       weaknesses: Set[String],
                       immunities: Set[String]
                      )

  case class Group(groupId: Int, units: List[GroupUnit])

  case class Army(groups: List[Group])

  def effectivePower(group: Group): Int =
    group.units.size * group.units.headOption.map(_.attack).getOrElse(0)

  def parseGroup(id: Int, groupStr: String): Group = {
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
    Group(id, units)
  }

  def parseArmies(lines: List[String]): List[Army] = {
    def parseGroups(armyGroupStrs: List[String]): List[Group] =
      armyGroupStrs.drop(1)
        .takeWhile("".!=)
        .zipWithIndex
        .map { case (groupStr, id) => parseGroup(id, groupStr) }

    val immuneSystemGroups = parseGroups(lines.dropWhile("Immune System:".!=))
    val immuneSystemArmy = Army(immuneSystemGroups)

    val infectionGroups = parseGroups(lines.dropWhile("Infection:".!=))
    val infectionArmy = Army(infectionGroups)

    List(immuneSystemArmy, infectionArmy)
  }

  //  def targetSelectionPhase(armies: List[Army])

  def fight() = {
    //    targetSelectionPhase()
    //    attackPhase()
  }

  def solvePart1(lines: List[String]): Int = ???

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day24-input")
    val solution1 = solvePart1(input)
    println(solution1)
  }

}
