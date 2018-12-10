package com.lukgru.algo.advent2018.day10

import com.lukgru.algo.advent2018.day10.TheStarsAlign.Point
import org.scalatest.FunSuite

class TheStarsAlignTest extends FunSuite {

  test("should parse point as string") {
    //given
    val line = "position=< 21842, -21524> velocity=<-2,  2>"

    //when
    val p = TheStarsAlign.parsePoint(line)

    //then
    assert(p == Point(21842, -21524, -2, 2))
  }

  test("should move point") {
    //given
    val p = Point(10, 20, -3, 15)

    //when
    val newP = TheStarsAlign.movePoint(p)

    //then
    assert(newP == Point(7, 35, -3, 15))
  }

  test("should print points") {
    //given
    val pointsStr = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>"
    val points = pointsStr.lines.map(TheStarsAlign.parsePoint).toList

    //when
    val printed = TheStarsAlign.printPoints(points)
    println(printed)

    //then
    val expected = "........#.............\n................#.....\n.........#.#..#.......\n......................\n#..........#.#.......#\n...............#......\n....#.................\n..#.#....#............\n.......#..............\n......#...............\n...#...#.#...#........\n....#..#..#.........#.\n.......#..............\n...........#..#.......\n#...........#.........\n...#.......#.........."
    assert(printed == expected)
  }
}
