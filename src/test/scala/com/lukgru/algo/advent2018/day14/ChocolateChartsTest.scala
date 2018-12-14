package com.lukgru.algo.advent2018.day14

import com.lukgru.algo.advent2018.day14.ChocolateCharts.Elf
import org.scalatest.FunSuite

class ChocolateChartsTest extends FunSuite {

  val initScoreBoard: Vector[Int] = Vector(3, 7)
  val initElves: Vector[Elf] = Vector(Elf(0), Elf(1))

  test("should return 10 recipes after 9") {
    //given
    val scoreboard = Vector(3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1, 5, 8, 9, 1, 6, 7, 7, 9, 2)

    //when
    val best = ChocolateCharts.getNAfterMRecipes(10)(9)(scoreboard)

    //then
    assert(best == Vector(5, 1, 5, 8, 9, 1, 6, 7, 7, 9))
  }

  test("should move elf according to score on its current position") {
    //given
    val elf = Elf(0)
    val scoreboard = Vector(1,2,3)

    //when
    val newElf = ChocolateCharts.moveElf(elf, scoreboard)

    //then
    assert(newElf.position == 2)
  }

  test("if end of scoreboard then elf should go to the beginning of the scoreboard") {
    //given
    val elf = Elf(3)
    val scoreboard = Vector(1,5,4,5,6,4,8)

    //when
    val newElf = ChocolateCharts.moveElf(elf, scoreboard)

    //then
    assert(newElf.position == 2)
  }


  test("should find 10 best after 5") {
    //when
    val best = ChocolateCharts.findBestNAfterM(10, initElves, initScoreBoard)(5)

    //then
    assert(best == Vector(0, 1, 2, 4, 5, 1, 5, 8, 9, 1))
  }

  test("should find 10 best after 18") {
    //when
    val best = ChocolateCharts.findBestNAfterM(10, initElves, initScoreBoard)(18)

    //then
    assert(best == Vector(9, 2, 5, 1, 0, 7, 1, 0, 8, 5))
  }

  test("should find 10 best after 2018") {
    //when
    val best = ChocolateCharts.findBestNAfterM(10, initElves, initScoreBoard)(2018)

    //then
    assert(best == Vector(5, 9, 4, 1, 4, 2, 9, 8, 8, 2))
  }

  test("should solve part 1") {
    //when
    val best = ChocolateCharts.findBestNAfterM(10, initElves, initScoreBoard)(47801)

    //then
    assert(best == Vector(1, 3, 4, 2, 3, 1, 6, 4, 1, 0))
  }

  test("should find 51589 after 9 recipes") {
    //given
    val lookingFor = Vector(5,1,5,8,9)

    //when
    val amountOnTheLeft = ChocolateCharts.findAfterHowManySequenceAppears(initElves, initScoreBoard)(lookingFor)

    //then
    assert(amountOnTheLeft == 9)
  }

  test("should find 01245 after 5 recipes") {
    //given
    val lookingFor = Vector(0,1,2,4,5)

    //when
    val amountOnTheLeft = ChocolateCharts.findAfterHowManySequenceAppears(initElves, initScoreBoard)(lookingFor)

    //then
    assert(amountOnTheLeft == 5)
  }

  test("should find 92510 after 18 recipes") {
    //given
    val lookingFor = Vector(9,2,5,1,0)

    //when
    val amountOnTheLeft = ChocolateCharts.findAfterHowManySequenceAppears(initElves, initScoreBoard)(lookingFor)

    //then
    assert(amountOnTheLeft == 18)
  }

  test("should find 59414 after 2018 recipes") {
    //given
    val lookingFor = Vector(5,9,4,1,4)

    //when
    val amountOnTheLeft = ChocolateCharts.findAfterHowManySequenceAppears(initElves, initScoreBoard)(lookingFor)

    //then
    assert(amountOnTheLeft == 2018)
  }

  test("should solve part 2") {
    //given
    val lookingFor = Vector(0,4,7,8,0,1)

    //when
    val amountOnTheLeft = ChocolateCharts.findAfterHowManySequenceAppears(initElves, initScoreBoard)(lookingFor)

    //then
    assert(amountOnTheLeft == 20235230)
  }

}
