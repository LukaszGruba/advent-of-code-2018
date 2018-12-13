package com.lukgru.algo.advent2018.day13

import com.lukgru.algo.advent2018.day13.MineCartMadness.Direction.Direction
import com.lukgru.algo.advent2018.day13.MineCartMadness.RoadOrientation.RoadOrientation
import com.lukgru.algo.advent2018.utils.InputLoader

object MineCartMadness {

  object Direction extends Enumeration {
    type Direction = Value
    val ^, <, v, > = Value
  }

  object RoadOrientation extends Enumeration {
    type RoadOrientation = Value
    val |, -, /, \, + = Value
  }

  case class Position(x: Int, y: Int)

  case class Cart(position: Position, direction: Direction)

  case class Road(position: Position, orientation: RoadOrientation)

  def parseDirection(c: Char): Direction = c match {
    case '^' => Direction.^
    case '>' => Direction.>
    case '<' => Direction.<
    case 'v' => Direction.v
  }

  def parseCarts(lines: List[String]): List[Cart] = {
    def isCart(c: Char): Boolean = "^v><".contains(c)

    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (points, y) =>
        points.map { case (c, x) => (c, x, y) }
      }
      .filter { case (c, _, _) => isCart(c) }
      .map { case (c, x, y) =>
        Cart(Position(x, y), parseDirection(c))
      }
  }

  def parseRoadOrientation(c: Char): RoadOrientation = c match {
    case '-' | '>' | '<' => RoadOrientation.-
    case '|' | '^' | 'v' => RoadOrientation.|
    case '/' => RoadOrientation./
    case '\\' => RoadOrientation.\
    case '+' => RoadOrientation.+
  }

  def parseRoads(lines: List[String]): Map[Position, Road] = {
    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (points, y) =>
        points.map { case (c, x) => (c, x, y) }
      }
      .filterNot { case (c, _, _) => c == ' ' }
      .map { case (c, x, y) =>
        val p = Position(x, y)
        p -> Road(p, parseRoadOrientation(c))
      }
      .toMap
  }

  def detectCollision(allCarts: List[Cart]): Option[Position] =
    allCarts.groupBy(_.position)
      .find { case (_, carts) => carts.length > 1 }
      .map { case (pos, _) => pos }

  def solvePart1(lines: List[String]): (Int, Int) = ???

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day13-input")
    val solution1 = solvePart1(input)
    println(solution1)
  }

}
