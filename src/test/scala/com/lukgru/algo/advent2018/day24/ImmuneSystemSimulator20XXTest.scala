package com.lukgru.algo.advent2018.day24

import com.lukgru.algo.advent2018.day24.ImmuneSystemSimulator20XX.GroupUnit
import org.scalatest.FunSuite

class ImmuneSystemSimulator20XXTest extends FunSuite {

  test("should parse group") {
    //given
    val groupStr = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"

    //when
    val group = ImmuneSystemSimulator20XX.parseGroup(123, groupStr)

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
}
