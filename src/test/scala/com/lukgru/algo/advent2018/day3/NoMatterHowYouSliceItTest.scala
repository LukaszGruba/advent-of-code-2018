package com.lukgru.algo.advent2018.day3

import com.lukgru.algo.advent2018.day3.NoMatterHowYouSliceIt.Rectangle
import org.scalatest.FunSuite

class NoMatterHowYouSliceItTest extends FunSuite {

  test("should parse rectangle") {
    //given
    val line = "#25 @ 936,937: 21x23"

    //when
    val rect = NoMatterHowYouSliceIt.parseRectangle(line)

    //then
    assert(rect == Rectangle(25, 936, 937, 21, 23))
  }

  test("should calc proper area points") {
    //given
    val rect = Rectangle(134, 1000, 500, 12, 23)

    //when
    val areaPoints = NoMatterHowYouSliceIt.areaPoints(rect)

    //then
    assert(areaPoints.size == 276)
  }

  test("should calc proper overlap area with 2 overlapping and 1 adjacent") {
    //given
    val rectangles = List(
      Rectangle(1, 1, 3, 4, 4),
      Rectangle(2, 3, 1, 4, 4),
      Rectangle(3, 5, 5, 2, 2)
    )

    //when
    val overlapArea = NoMatterHowYouSliceIt.calcOverlapArea(rectangles)

    //then
    assert(overlapArea == 4)
  }

  test("should calc proper overlap area with 3 overlapping") {
    //given
    val rectangles = List(
      Rectangle(1, 1, 3, 4, 4),
      Rectangle(2, 3, 1, 4, 4),
      Rectangle(3, 0, 0, 100, 100)
    )

    //when
    val overlapArea = NoMatterHowYouSliceIt.calcOverlapArea(rectangles)

    //then
    assert(overlapArea == 28)
  }

}
