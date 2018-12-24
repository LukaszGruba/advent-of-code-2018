package com.lukgru.algo.advent2018.day22

import com.lukgru.algo.advent2018.day22.ModeMaze.RegionType.RegionType

import scala.collection.mutable

object ModeMaze {

  val depth = 7740
  val caveEntryPos = Position(0, 0)
  val targetPos = Position(12, 763)

  object RegionType extends Enumeration {
    type RegionType = Value
    val Rocky, Narrow, Wet = Value
  }

  case class Position(x: Int, y: Int)

  case class Region(pos: Position) {
    lazy val regionType: RegionType = determineRegionType(calcErosionLevel(pos))
  }

  def calcGeologicIndex(regPos: Position): Long = regPos match {
    case `caveEntryPos` => 0
    case `targetPos` => 0
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
        println(s"cache updated for: ($pos, $erosionLevel")
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

  def solvePart1(): Long = {
    val areaToCalc = getArea(caveEntryPos, targetPos)
    areaToCalc.map(calcErosionLevel)
      .map(determineRegionType)
      .map(getRiskLevel)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val solution1 = solvePart1()
    println(solution1)
  }

}