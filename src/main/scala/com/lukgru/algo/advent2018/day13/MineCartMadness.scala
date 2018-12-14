package com.lukgru.algo.advent2018.day13

import com.lukgru.algo.advent2018.day13.MineCartMadness.Direction.Direction
import com.lukgru.algo.advent2018.day13.MineCartMadness.JunctionManeuver.JunctionManeuver
import com.lukgru.algo.advent2018.day13.MineCartMadness.RoadOrientation.RoadOrientation
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

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

  def eliminateCollisions(allCarts: List[Cart]): List[Cart] = {
    allCarts.groupBy(_.position)
      .filterNot { case (_, cartsWithSamePos) => cartsWithSamePos.length > 1 }
      .map { case (pos, carts) => (pos, carts.head) }
      .values
      .toList
  }

  def sortByPosition(carts: List[Cart]): List[Cart] = carts.sortBy(c => (c.position.y, c.position.x))

  object counter {
    private var i = 0

    def ++(): Unit = i += 1

    def value(): Int = i
  }

  //  def findLastCartStanding(carts: List[Cart], roads: Map[Position, Road]): Cart = {
  //    val moveF = move(roads) _
  //    @tailrec
  //    def find(toMove: List[Cart], moved: List[Cart]): Cart = {
  //      printState(toMove ++ moved, roads)
  //      counter.++()
  //      if ((toMove ++ moved).length == 1) {
  //        if (toMove.length == 1) toMove.head//moveF(toMove.head)
  //        else moved.head
  //      }
  //      else if (toMove.isEmpty) {
  //        find(sortByPosition(eliminateCollisions(moved)), List.empty)
  //      }
  //      else {
  //        val afterFirstIsMoved = toMove.updated(0, moveF(toMove.head))
  //        val withoutCollisions = sortByPosition(eliminateCollisions(afterFirstIsMoved))
  //        if (withoutCollisions.head == afterFirstIsMoved.head) {
  //          find(withoutCollisions.tail, moved :+ withoutCollisions.head)
  //        }
  //        else {
  //          find(withoutCollisions, moved)
  //        }
  //      }
  //    }
  //    find(carts, List.empty)
  //  }

  def findCollisions(carts: List[Cart]): List[Position] =
    carts.map(_.position)
      .groupBy(identity)
      .values
      .filter(_.length > 1)
      .map(_.head)
      .toList

  @tailrec
  def findLastCartStanding(carts: List[Cart], roads: Map[Position, Road]): Cart = {
    if (carts.length == 1) carts.head
    else {
      val moveF = move(roads) _
      var toMove = sortByPosition(carts)
      var moved = List.empty[Cart]
      while (toMove.nonEmpty) {
        val c = toMove.head
        toMove = toMove.tail
        val movedCart = moveF(c)
        moved = moved :+ movedCart
        val collisions = findCollisions(moved ++ toMove)
        moved = moved.filterNot(c => collisions.contains(c.position))
        toMove = sortByPosition(toMove.filterNot(c => collisions.contains(c.position)))
      }
      val sortedRemaining = sortByPosition(moved)
      findLastCartStanding(sortedRemaining, roads)
    }
  }

  def printState(carts: List[Cart], roads: Map[Position, Road]): Unit = {
    def formatDirection(dir: Direction): String = dir match {
      case Direction.< => "<"
      case Direction.^ => "^"
      case Direction.> => ">"
      case Direction.v => "v"
    }

    def formatOrientation(or: RoadOrientation): String = or match {
      case RoadOrientation./ => "/"
      case RoadOrientation.| => "|"
      case RoadOrientation.- => "-"
      case RoadOrientation.\ => "\\"
      case RoadOrientation.+ => "+"
    }

    val maxX = roads.keys.maxBy(p => p.x).x
    val maxY = roads.keys.maxBy(p => p.y).y
    val sb = new StringBuilder()
    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        val road = roads.get(Position(x, y))
          .map(r => formatOrientation(r.orientation))
          .getOrElse(" ")
        val sign = carts.find(c => c.position == Position(x, y)).map(c => formatDirection(c.direction)).getOrElse(road)
        sb.append(sign)
      }
      sb.append('\n')
    }
    println(sb.mkString)
  }

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

  def solvePart2(lines: List[String]): (Int, Int) = {
    val carts = parseCarts(lines)
    val roads = parseRoads(lines)
    val lastCartStanding = findLastCartStanding(sortByPosition(carts), roads)
    printState(List(lastCartStanding), roads)
    println(counter.value)
    (lastCartStanding.position.x, lastCartStanding.position.y)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day13-input")
    val solution1 = solvePart1(input)
    println(solution1)

    val solution2 = solvePart2(input)
    println(solution2)
  }

}
