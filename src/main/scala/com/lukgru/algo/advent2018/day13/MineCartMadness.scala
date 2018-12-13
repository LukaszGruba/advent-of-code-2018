package com.lukgru.algo.advent2018.day13

import com.lukgru.algo.advent2018.day13.MineCartMadness.Direction.Direction
import com.lukgru.algo.advent2018.day13.MineCartMadness.JunctionManeuver.JunctionManeuver
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

  object JunctionManeuver extends Enumeration {
    type JunctionManeuver = Value
    val Left, Straight, Right = Value
  }

  case class Position(x: Int, y: Int)

  case class Cart(position: Position, direction: Direction, lastJunctionManeuver: JunctionManeuver = JunctionManeuver.Right)

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

  def turnLeft(direction: Direction): Direction = direction match {
    case Direction.< => Direction.v
    case Direction.^ => Direction.<
    case Direction.> => Direction.^
    case Direction.v => Direction.>
  }

  def turnRight(direction: Direction): Direction = direction match {
    case Direction.< => Direction.^
    case Direction.^ => Direction.>
    case Direction.> => Direction.v
    case Direction.v => Direction.<
  }

  def crossJunction(cart: Cart): (Direction, JunctionManeuver) = cart.lastJunctionManeuver match {
    case JunctionManeuver.Left => (cart.direction, JunctionManeuver.Straight)
    case JunctionManeuver.Straight => (turnRight(cart.direction), JunctionManeuver.Right)
    case JunctionManeuver.Right => (turnLeft(cart.direction), JunctionManeuver.Left)
  }

  def nextDirection(cart: Cart, roadOrientation: RoadOrientation): (Direction, JunctionManeuver) =
    (cart.direction, roadOrientation) match {
      case (_, RoadOrientation.-) |
           (_, RoadOrientation.|) => (cart.direction, cart.lastJunctionManeuver)
      case (Direction.>, RoadOrientation./) |
           (Direction.^, RoadOrientation.\) |
           (Direction.<, RoadOrientation./) |
           (Direction.v, RoadOrientation.\) => (turnLeft(cart.direction), cart.lastJunctionManeuver)
      case (Direction.>, RoadOrientation.\) |
           (Direction.^, RoadOrientation./) |
           (Direction.<, RoadOrientation.\) |
           (Direction.v, RoadOrientation./) => (turnRight(cart.direction), cart.lastJunctionManeuver)
      case (_, RoadOrientation.+) => crossJunction(cart)
    }

  def nextPosition(position: Position, direction: Direction): Position = direction match {
    case Direction.< => position.copy(x = position.x - 1)
    case Direction.^ => position.copy(y = position.y - 1)
    case Direction.> => position.copy(x = position.x + 1)
    case Direction.v => position.copy(y = position.y + 1)
  }

  def move(road: Map[Position, Road])(cart: Cart): Cart = {
    val currentRoad = road(cart.position)
    val (cartsNewDirection, lastJunctionManeuver) = nextDirection(cart, currentRoad.orientation)
    val cartsNewPosition = nextPosition(cart.position, cartsNewDirection)
    Cart(cartsNewPosition, cartsNewDirection, lastJunctionManeuver)
  }

  def detectCollision(allCarts: List[Cart]): Option[Position] =
    allCarts.groupBy(_.position)
      .find { case (_, carts) => carts.length > 1 }
      .map { case (pos, _) => pos }

  def solvePart1(lines: List[String]): (Int, Int) = {
    var carts = parseCarts(lines)
    val road = parseRoads(lines)
    val moveF = move(road) _

    //TODO: get rid of var
    while (detectCollision(carts).isEmpty) {
      carts = carts.map(moveF)
    }
    val collisionPosition = detectCollision(carts).get
    (collisionPosition.x, collisionPosition.y)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day13-input")
    val solution1 = solvePart1(input)
    println(solution1)
  }

}
