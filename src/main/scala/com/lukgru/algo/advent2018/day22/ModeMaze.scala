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
        println(s"cache updated with: ($pos, $erosionLevel)")
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

  def createNewPaths(current: TripState): Vector[TripState] = {
    val adjacentPositions = adjacentPos(current.path.last)
    val possibleMoves = for (pos <- adjacentPositions; possibleEq <- Equipment.values.toVector) yield (pos, possibleEq)
    possibleMoves.map { case (newPos, eq) =>
      val path = current.path :+ newPos
      val time = if (eq != current.currentEquipment) {
        current.time + 1 + changeCost
      } else current.time + 1
      TripState(time, path, eq)
    }.toVector
  }

  @tailrec
  def findShortestTrip(tripStates: Vector[TripState]): TripState = {
    if (tripStates.size % 1000 == 0) println(tripStates.size)
    val (head, tail) = (tripStates.head, tripStates.tail)
    if (head.path.last == targetPos) {
      if (head.currentEquipment == Equipment.Torch) head
      else {
        val newTail = TripState(head.time + changeCost, head.path, Equipment.Torch) +: tail
        findShortestTrip(newTail.sortBy(_.time))
      }
    }
    else {
      val newPaths = createNewPaths(head)
      val newTail = newPaths ++ tail
      findShortestTrip(newTail.sortBy(_.time))
    }
  }

  def solvePart2(): Long = {
    val TripState(time, _, _) = findShortestTrip(Vector(TripState(0, Vector(caveEntryPos), Equipment.Torch)))
    time
  }

  def main(args: Array[String]): Unit = {
    val depthP = 7740
    val caveEntryPosP = Position(0, 0)
    val targetPosP = Position(12, 763)

    val solution1 = solvePart1(depthP, caveEntryPosP, targetPosP)
    println(solution1)

    val solution2 = solvePart2()
    println(solution2)
  }

}