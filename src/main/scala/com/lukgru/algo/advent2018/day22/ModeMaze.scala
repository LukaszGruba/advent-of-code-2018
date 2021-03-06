package com.lukgru.algo.advent2018.day22

import com.lukgru.algo.advent2018.day22.ModeMaze.Equipment.Equipment
import com.lukgru.algo.advent2018.day22.ModeMaze.RegionType.RegionType

import scala.annotation.tailrec
import scala.collection.mutable

object ModeMaze {

  var depth = 0
  var caveEntryPos = Position(0, 0)
  var targetPos = Position(0, 0)

  val changeCost = 7

  object RegionType extends Enumeration {
    type RegionType = Value
    val Rocky, Narrow, Wet = Value
  }

  object Equipment extends Enumeration {
    type Equipment = Value
    val Neither, ClimbingGear, Torch = Value
  }

  case class TripState(time: Int, path: Vector[Position], currentEquipment: Equipment)

  case class Position(x: Int, y: Int)

  case class Region(pos: Position) {
    lazy val regionType: RegionType = determineRegionType(calcErosionLevel(pos))
  }

  def calcGeologicIndex(regPos: Position): Long = regPos match {
    case _ if regPos == caveEntryPos => 0
    case _ if regPos == targetPos => 0
    case Position(x, 0) => x * 16807
    case Position(0, y) => y * 48271
    case Position(x, y) => calcErosionLevel(Position(x - 1, y)) * calcErosionLevel(Position(x, y - 1))
  }

  val calcErosionLevel: Position => Long = memoizedErosionLevel

  def memoizedErosionLevel: Position => Long = {
    def calcErosionLevel(regPos: Position): Long = {
      val geoIndex = calcGeologicIndex(regPos)
      (geoIndex + depth) % 20183
    }

    val cache = mutable.Map.empty[Position, Long]

    pos =>
      cache.getOrElse(pos, {
        cache.update(pos, calcErosionLevel(pos))
        val erosionLevel = cache(pos)
        if (pos.y % 100 == 0) println(pos)
//        println(s"cache updated with: ($pos, $erosionLevel)")
        erosionLevel
      })
  }

  def determineRegionType(erosionLevel: Long): RegionType = erosionLevel % 3 match {
    case 0 => RegionType.Rocky
    case 1 => RegionType.Wet
    case 2 => RegionType.Narrow
  }

  def getRiskLevel(regionType: RegionType): Int = regionType match {
    case RegionType.Rocky => 0
    case RegionType.Wet => 1
    case RegionType.Narrow => 2
  }

  def getArea(entry: Position, target: Position): List[Position] = {
    val area =
      for (x <- entry.x to target.x; y <- entry.y to target.y)
        yield Position(x, y)
    area.toList
  }

  def solvePart1(depthP: Int, entrance: Position, target: Position): Long = {
    this.depth = depthP
    this.caveEntryPos = entrance
    this.targetPos = target

    val areaToCalc = getArea(caveEntryPos, targetPos)
    areaToCalc.map(calcErosionLevel)
      .map(determineRegionType)
      .map(getRiskLevel)
      .sum
  }

  def adjacentPos(pos: Position): List[Position] = {
    val Position(x, y) = pos
    List(
      Position(x + 1, y),
      Position(x - 1, y),
      Position(x, y + 1),
      Position(x, y - 1)
    ).filter(p => p.x >= 0 && p.y >= 0)
  }

  def allowedEquipment(pos: Position): List[Equipment] =
    determineRegionType(calcErosionLevel(pos)) match {
      case RegionType.Narrow => List(Equipment.Torch, Equipment.Neither)
      case RegionType.Wet => List(Equipment.Neither, Equipment.ClimbingGear)
      case RegionType.Rocky => List(Equipment.ClimbingGear, Equipment.Torch)
    }

  def createNewPaths(current: TripState, visited: Set[(Position, Equipment)]): Vector[TripState] = {
    val adjacentPositions = adjacentPos(current.path.last).filterNot(current.path.contains)
    val possibleMoves = for (pos <- adjacentPositions; possibleEq <- Equipment.values.toVector) yield (pos, possibleEq)
    possibleMoves
      .filter { case (newPos, eq) => allowedEquipment(newPos).contains(eq) }
      .filterNot(visited.contains)
      .map { case (newPos, eq) =>
        val path = current.path :+ newPos
        val time = if (eq != current.currentEquipment) {
          current.time + 1 + changeCost
        } else current.time + 1
        TripState(time, path, eq)
      }.toVector
  }

  def eliminateAndSort(tripStates: Vector[TripState]): Vector[TripState] = {
    def heuristic(p: Position): Int = {
      val dx = Math.abs(p.x - targetPos.x)
      val dy = Math.abs(p.y - targetPos.y)
      Math.sqrt(Math.pow(dx, 2) + Math.pow(dy, 2)).toInt
    }
    tripStates.groupBy(state => (state.path.last, state.currentEquipment))
      .map { case (_, states) => states.minBy(s => s.time + heuristic(s.path.last)) }
      .toVector
      .sortBy(_.time)
  }

  @tailrec
  def findShortestTrip(tripStates: Vector[TripState], visited: Set[(Position, Equipment)]): TripState = {
//    if (tripStates.size % 1000 < 10) println(tripStates.size)
    val (head, tail) = (tripStates.head, tripStates.tail)
    if (head.path.last == targetPos) {
      if (head.currentEquipment == Equipment.Torch) head
      else {
        val newTail = TripState(head.time + changeCost, head.path, Equipment.Torch) +: tail
        findShortestTrip(eliminateAndSort(newTail), visited + ((head.path.last, head.currentEquipment)))
      }
    }
    else {
      val newPaths = createNewPaths(head, visited)
      val newTail =
        if (newPaths.isEmpty) tail
        else newPaths ++ tail
      findShortestTrip(eliminateAndSort(newTail), visited + ((head.path.last, head.currentEquipment)))
    }
  }

  def solvePart2(depthP: Int, entrance: Position, target: Position): Long = {
    this.depth = depthP
    this.caveEntryPos = entrance
    this.targetPos = target

    val TripState(time, _, _) = findShortestTrip(Vector(TripState(0, Vector(caveEntryPos), Equipment.Torch)), Set.empty)
    time
  }

  def main(args: Array[String]): Unit = {
    val depthP = 7740
    val caveEntryPosP = Position(0, 0)
    val targetPosP = Position(12, 763)

//    val solution1 = solvePart1(depthP, caveEntryPosP, targetPosP)
//    println(solution1)

    val solution2 = solvePart2(depthP, caveEntryPosP, targetPosP)
    println(solution2)
  }

}