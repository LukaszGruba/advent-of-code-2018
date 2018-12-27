package com.lukgru.algo.advent2018.day24

import com.lukgru.algo.advent2018.day24.ImmuneSystemSimulator20XX.GroupUnit
import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class ImmuneSystemSimulator20XXTest extends FunSuite {

  test("should parse group") {
    //given
    val groupStr = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"

    //when
    val group = ImmuneSystemSimulator20XX.parseGroup(1)(123, groupStr)

    //then
    assert(group.groupId == 123)
    assert(group.units.length == 18)
    assert(group.units.head == GroupUnit(729, 8, "radiation", 10, Set("fire"), Set("cold", "slashing")))
  }

  test("should parse armies") {
    //given
    val armiesStrs = List(
      "Immune System:",
      "2749 units each with 8712 hit points (immune to radiation, cold; weak to fire) with an attack that does 30 radiation damage at initiative 18",
      "704 units each with 1890 hit points with an attack that does 26 fire damage at initiative 17",
      "",
      "Infection:",
      "1230 units each with 11944 hit points (weak to cold) with an attack that does 17 bludgeoning damage at initiative 1",
      "7588 units each with 53223 hit points (immune to bludgeoning) with an attack that does 13 cold damage at initiative 12",
    )

    //when
    val armies = ImmuneSystemSimulator20XX.parseArmies(armiesStrs)

    //then
    assert(armies.size == 2)
    val immuneSystemArmy = armies.head
    val infectionArmy = armies(1)

    assert(immuneSystemArmy.groups.length == 2)
    assert(immuneSystemArmy.groups.map(_.units.length).sum == 3453)

    assert(infectionArmy.groups.length == 2)
    assert(infectionArmy.groups.map(_.units.length).sum == 8818)
  }

  test("should solve part 1 for example") {
    //given
    val input = List(
      "Immune System:",
      "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2",
      "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3",
      "",
      "Infection:",
      "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1",
      "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4",
    )

    //when
    val solution = ImmuneSystemSimulator20XX.solvePart1(input)

    //then
    assert(solution == 5216)
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day24-input")

    //when
    val solution = ImmuneSystemSimulator20XX.solvePart1(input)

    //then
    assert(solution == 15470)
  }
}
