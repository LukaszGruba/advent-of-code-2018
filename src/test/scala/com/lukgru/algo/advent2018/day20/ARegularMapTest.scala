package com.lukgru.algo.advent2018.day20

import com.lukgru.algo.advent2018.day20.ARegularMap.{p, parseRegMapPositions}
import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class ARegularMapTest extends FunSuite {

  test("should parse simple small RegMap") {
    //given
    val mapStr = "^ENWWW(NEEE|SSE(EE|N))$"
    //    #########
    //    #.|.|.|.#
    //    #-#######
    //    #.|.|.|.#
    //    #-#####-#
    //    #.#.#X|.#
    //    #-#-#####
    //    #.|.|.|.#
    //    #########

    //when
    val regMap = parseRegMapPositions(mapStr)

    //then
    assert(regMap.rooms.map(_.pos) == Set(
      p(-4, 4), p(-2, 4), p(0, 4), p(2, 4),
      p(-4, 2), p(-2, 2), p(0, 2), p(2, 2),
      p(-4, 0), p(-2, 0), p(2, 0),
      p(-4, -2), p(-2, -2), p(0, -2), p(2, -2)
    ))
    assert(regMap.doors.map(_.pos) == Set(
      p(-3, 4), p(-1, 4), p(1, 4),
      p(-4, 3),
      p(-3, 2), p(-1, 2), p(1, 2),
      p(-4, 1), p(2, 1),
      p(1, 0),
      p(-4, -1), p(-2, -1),
      p(-3, -2), p(-1, -2), p(1, -2)
    ))
  }

  test("should parse more complex RegMap") {
    //given
    val mapStr = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
    //    ###############
    //    #.|.|.|.#.|.|.#
    //    #-###-###-#-#-#
    //    #.|.#.|.|.#.#.#
    //    #-#########-#-#
    //    #.#.|.|.|.|.#.#
    //    #-#-#########-#
    //    #.#.#.|X#.|.#.#
    //    ###-#-###-#-#-#
    //    #.|.#.#.|.#.|.#
    //    #-###-#####-###
    //    #.|.#.|.|.#.#.#
    //    #-#-#####-#-#-#
    //    #.#.|.|.|.#.|.#
    //    ###############

    //when
    val regMap = parseRegMapPositions(mapStr)

    //then
    assert(regMap.rooms.map(_.pos) == Set(
      p(-6, 6), p(-4, 6), p(-2, 6), p(0, 6), p(2, 6), p(4, 6), p(6, 6),
      p(-6, 4), p(-4, 4), p(-2, 4), p(0, 4), p(2, 4), p(4, 4), p(6, 4),
      p(-6, 2), p(-4, 2), p(-2, 2), p(0, 2), p(2, 2), p(4, 2), p(6, 2),
      p(-6, 0), p(-4, 0), p(-2, 0), p(2, 0), p(4, 0), p(6, 0),
      p(-6, -2), p(-4, -2), p(-2, -2), p(0, -2), p(2, -2), p(4, -2), p(6, -2),
      p(-6, -4), p(-4, -4), p(-2, -4), p(0, -4), p(2, -4), p(4, -4), p(6, -4),
      p(-6, -6), p(-4, -6), p(-2, -6), p(0, -6), p(2, -6), p(4, -6), p(6, -6),
    ))
    assert(regMap.doors.map(_.pos) == Set(
      p(-5, 6), p(-3, 6), p(-1, 6), p(3, 6), p(5, 6),
      p(-6, 5), p(-2, 5), p(2, 5), p(4, 5), p(6, 5),
      p(-5, 4), p(-1, 4), p(1, 4),
      p(-6, 3), p(4, 3), p(6, 3),
      p(-3, 2), p(-1, 2), p(1, 2), p(3, 2),
      p(-6, 1), p(-4, 1), p(6, 1),
      p(-1, 0), p(3, 0),
      p(-4, -1), p(-2, -1), p(2, -1), p(4, -1), p(6, -1),
      p(-5, -2), p(1, -2), p(5, -2),
      p(-6, -3), p(-2, -3), p(4, -3),
      p(-5, -4), p(-1, -4), p(1, -4),
      p(-6, -5), p(-4, -5), p(2, -5), p(4, -5), p(6, -5),
      p(-3, -6), p(-1, -6), p(1, -6), p(5, -6)
    ))
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day20-input").head

    //when
    val shortestPathLengthToMostDistantRoom = ARegularMap.findShortestPathLengthToMostDistantRoom(input)

    //then
    assert(shortestPathLengthToMostDistantRoom == 3788)
  }

}
