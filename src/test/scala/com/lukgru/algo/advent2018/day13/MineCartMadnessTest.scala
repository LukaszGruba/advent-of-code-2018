package com.lukgru.algo.advent2018.day13

import com.lukgru.algo.advent2018.day13.MineCartMadness.RoadOrientation.RoadOrientation
import com.lukgru.algo.advent2018.day13.MineCartMadness._
import com.lukgru.algo.advent2018.utils.InputLoader
import org.scalatest.FunSuite

class MineCartMadnessTest extends FunSuite {

  test("should detect collision if present") {
    //given
    val carts = List(
      Cart(Position(19, 47), Direction.v),
      Cart(Position(10, 48), Direction.^),
      Cart(Position(19, 47), Direction.>)
    )

    //when
    val collision = detectCollision(carts)

    //then
    assert(collision.isDefined)
    assert(collision.get == Position(19, 47))
  }

  test("should not detect collision if there is none") {
    //given
    val carts = List(
      Cart(Position(19, 47), Direction.v),
      Cart(Position(10, 48), Direction.^)
    )

    //when
    val collision = detectCollision(carts)

    //then
    assert(collision.isEmpty)
  }

  test("should parse carts") {
    //given
    val input = List(
      "/->-\\",
      "|   |  /----\\",
      "| /-+--+-\\  |",
      "| | |  | v  |",
      "\\-+-/  \\-+--/",
      "\\------/"
    )

    //when
    val carts = parseCarts(input)

    //then
    assert(carts == List(Cart(Position(2, 0), Direction.>), Cart(Position(9, 3), Direction.v)))
  }

  test("should parse roads") {
    //given
    val input = List(
      "/->-\\",
      "|   |  /----\\",
      "| /-+--+-\\  |",
      "| | |  | v  |",
      "\\-+-/  \\-+--/",
      "\\------/"
    )

    //when
    val roads = parseRoads(input)

    //then
    val expectedMap = Map(
      roadEntry(0, 0, RoadOrientation./),
      roadEntry(1, 0, RoadOrientation.-),
      roadEntry(2, 0, RoadOrientation.-),
      roadEntry(3, 0, RoadOrientation.-),
      roadEntry(4, 0, RoadOrientation.\),

      roadEntry(0, 1, RoadOrientation.|),
      roadEntry(4, 1, RoadOrientation.|),
      roadEntry(7, 1, RoadOrientation./),
      roadEntry(8, 1, RoadOrientation.-),
      roadEntry(9, 1, RoadOrientation.-),
      roadEntry(10, 1, RoadOrientation.-),
      roadEntry(11, 1, RoadOrientation.-),
      roadEntry(12, 1, RoadOrientation.\),

      roadEntry(0, 2, RoadOrientation.|),
      roadEntry(2, 2, RoadOrientation./),
      roadEntry(3, 2, RoadOrientation.-),
      roadEntry(4, 2, RoadOrientation.+),
      roadEntry(5, 2, RoadOrientation.-),
      roadEntry(6, 2, RoadOrientation.-),
      roadEntry(7, 2, RoadOrientation.+),
      roadEntry(8, 2, RoadOrientation.-),
      roadEntry(9, 2, RoadOrientation.\),
      roadEntry(12, 2, RoadOrientation.|),

      roadEntry(0, 3, RoadOrientation.|),
      roadEntry(2, 3, RoadOrientation.|),
      roadEntry(4, 3, RoadOrientation.|),
      roadEntry(7, 3, RoadOrientation.|),
      roadEntry(9, 3, RoadOrientation.|),
      roadEntry(12, 3, RoadOrientation.|),

      roadEntry(0, 4, RoadOrientation.\),
      roadEntry(1, 4, RoadOrientation.-),
      roadEntry(2, 4, RoadOrientation.+),
      roadEntry(3, 4, RoadOrientation.-),
      roadEntry(4, 4, RoadOrientation./),
      roadEntry(7, 4, RoadOrientation.\),
      roadEntry(8, 4, RoadOrientation.-),
      roadEntry(9, 4, RoadOrientation.+),
      roadEntry(10, 4, RoadOrientation.-),
      roadEntry(11, 4, RoadOrientation.-),
      roadEntry(12, 4, RoadOrientation./),

      roadEntry(0, 5, RoadOrientation.\),
      roadEntry(1, 5, RoadOrientation.-),
      roadEntry(2, 5, RoadOrientation.-),
      roadEntry(3, 5, RoadOrientation.-),
      roadEntry(4, 5, RoadOrientation.-),
      roadEntry(5, 5, RoadOrientation.-),
      roadEntry(6, 5, RoadOrientation.-),
      roadEntry(7, 5, RoadOrientation./)
    )
    assert(roads == expectedMap)
  }

  test("should go through simple line") {
    //given
    val cart = Cart(Position(5, 3), Direction.v)
    val road = Map(
      roadEntry(5, 3, RoadOrientation.|),
      roadEntry(5, 4, RoadOrientation.\),
      roadEntry(6, 4, RoadOrientation.-),
      roadEntry(7, 4, RoadOrientation.\),
      roadEntry(7, 5, RoadOrientation.|),
      roadEntry(7, 6, RoadOrientation.+),
      roadEntry(8, 6, RoadOrientation.-)
    )

    //when
    val endingCartState = (1 to road.size).foldLeft(cart) { case (prevCart, _) => move(road)(prevCart) }

    //then
    assert(endingCartState == Cart(Position(9, 6), Direction.>, JunctionManeuver.Left))
  }

  test("should solve part 1") {
    //given
    val input = InputLoader.loadLines("day13-input")

    //when
    val solution = solvePart1(input)

    //then
    assert(solution == (103, 85))
  }

  test("should find last cart standing for simple example") {
    //given
    val input = List(
      "/>-<\\",
      "|   |",
      "| /<+-\\",
      "| | | v",
      "\\>+</ |",
      "  |   ^",
      "  \\<->/"
    )

    //when
    val lastCartStandingPosition = solvePart2(input)

    //then
    assert(lastCartStandingPosition == (6, 4))
  }

  test("should sort by position") {
    def c(x: Int, y: Int): Cart = Cart(Position(x, y), Direction.<)
    //given
    val unordered = List(
      c(4,3),
      c(1,2),
      c(0,3),
      c(2,2),
      c(0,1),
      c(0,0),
      c(1,1),
      c(1,0),
    )

    //when
    val ordered = sortByPosition(unordered)

    //then
    val expected = List(
      c(0,0),
      c(1,0),
      c(0,1),
      c(1,1),
      c(1,2),
      c(2,2),
      c(0,3),
      c(4,3)
    )
    assert(ordered == expected)
  }

  test("should solve part 2") {
    //given
    val input = InputLoader.loadLines("day13-input")

    //when
    val solution = solvePart2(input)

    //then
    assert(solution == (88,64))
  }

  def roadEntry(x: Int, y: Int, orientation: RoadOrientation): (Position, Road) = {
    val p = Position(x, y)
    p -> Road(p, orientation)
  }

}
