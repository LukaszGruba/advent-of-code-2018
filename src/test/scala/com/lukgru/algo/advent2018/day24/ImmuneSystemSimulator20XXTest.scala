package com.lukgru.algo.advent2018.day24

import com.lukgru.algo.advent2018.day24.ImmuneSystemSimulator20XX.GroupUnit
import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class ImmuneSystemSimulator20XXTest extends FunSuite {

  test("should parse group") {
    //given
    val groupStr = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"

    //when
    val group = ImmuneSystemSimulator20XX.parseGroup("Infection")(123, groupStr)

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

  test("should solve part 2 for example") {
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
    val solution = ImmuneSystemSimulator20XX.solvePart2(input)

    //then
    assert(solution == 51)
  }

  test("should solve part 2 for someone else's input") {
    //given
    val input = List(
      "Immune System:",
      "2987 units each with 5418 hit points (immune to slashing; weak to cold, bludgeoning) with an attack that does 17 cold damage at initiative 5",
      "1980 units each with 9978 hit points (immune to cold) with an attack that does 47 cold damage at initiative 19",
      "648 units each with 10733 hit points (immune to radiation, fire, slashing) with an attack that does 143 fire damage at initiative 9",
      "949 units each with 3117 hit points with an attack that does 29 fire damage at initiative 10",
      "5776 units each with 5102 hit points (weak to cold; immune to slashing) with an attack that does 8 radiation damage at initiative 15",
      "1265 units each with 4218 hit points (immune to radiation) with an attack that does 24 radiation damage at initiative 16",
      "3088 units each with 10066 hit points (weak to slashing) with an attack that does 28 slashing damage at initiative 1",
      "498 units each with 1599 hit points (immune to bludgeoning; weak to radiation) with an attack that does 28 bludgeoning damage at initiative 11",
      "3705 units each with 10764 hit points with an attack that does 23 cold damage at initiative 7",
      "3431 units each with 3666 hit points (weak to slashing; immune to bludgeoning) with an attack that does 8 bludgeoning damage at initiative 8",
      "",
      "Infection:",
      "2835 units each with 33751 hit points (weak to cold) with an attack that does 21 bludgeoning damage at initiative 13",
      "4808 units each with 32371 hit points (weak to radiation; immune to bludgeoning) with an attack that does 11 cold damage at initiative 14",
      "659 units each with 30577 hit points (weak to fire; immune to radiation) with an attack that does 88 slashing damage at initiative 12",
      "5193 units each with 40730 hit points (immune to radiation, fire, bludgeoning; weak to slashing) with an attack that does 14 cold damage at initiative 20",
      "1209 units each with 44700 hit points (weak to bludgeoning, radiation) with an attack that does 71 fire damage at initiative 18",
      "6206 units each with 51781 hit points (immune to cold) with an attack that does 13 fire damage at initiative 4",
      "602 units each with 22125 hit points (weak to radiation, bludgeoning) with an attack that does 73 cold damage at initiative 3",
      "5519 units each with 37123 hit points (weak to slashing, fire) with an attack that does 12 radiation damage at initiative 2",
      "336 units each with 23329 hit points (weak to fire; immune to cold, bludgeoning, radiation) with an attack that does 134 cold damage at initiative 17",
      "2017 units each with 50511 hit points (immune to bludgeoning) with an attack that does 42 fire damage at initiative 6",
    )

    //when
    val solution = ImmuneSystemSimulator20XX.solvePart2(input)

    //then
    assert(solution == 434)
  }

  test("should solve part 2") {
    //given
    val input = InputLoader.loadLines("day24-input")

    //when
    val solution = ImmuneSystemSimulator20XX.solvePart2(input)

    //then
    assert(solution == 5742)
  }
}
