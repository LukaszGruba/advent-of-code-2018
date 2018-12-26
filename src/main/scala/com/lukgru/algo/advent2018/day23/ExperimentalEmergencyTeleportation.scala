package com.lukgru.algo.advent2018.day23

import java.util.Date

import com.lukgru.algo.advent2018.utils.InputLoader

object ExperimentalEmergencyTeleportation {

  case class Position(x: Int, y: Int, z: Int)

  case class Nanobot(pos: Position, signalRadius: Int)

  case class Region(xs: Range, ys: Range, zs: Range)

  def parseNanobots(lines: List[String]): Set[Nanobot] = {
    val nanobotPattern = "pos=<(-?\\d*),(-?\\d*),(-?\\d*)>, r=(-?\\d*)".r
    lines.map { line =>
      val nanobotPattern(x, y, z, r) = line
      Nanobot(Position(x.toInt/1000, y.toInt/1000, z.toInt/1000), r.toInt/1000)
    }.toSet
  }

  def calcDistance(p1: Position, p2: Position): Int =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) + Math.abs(p1.z - p2.z)

  def isInRange(nanobot: Nanobot, dest: Position): Boolean =
    nanobot.signalRadius >= calcDistance(nanobot.pos, dest)

  def getAllInRangeOf(allNanobots: Set[Nanobot])(nanobot: Nanobot): Set[Nanobot] =
    allNanobots.filter(n => isInRange(nanobot, n.pos))

  def findTheStrongestNanobot(allNanobots: Set[Nanobot]): Nanobot =
    allNanobots.maxBy(_.signalRadius)

  def countNanobotsInRangeOfTheStrongestOne(lines: List[String]): Int = {
    val nanobots = parseNanobots(lines)
    val strongest = findTheStrongestNanobot(nanobots)
    val inRangeOfTheStrongest = getAllInRangeOf(nanobots)(strongest)
    inRangeOfTheStrongest.size
  }

  def getBoundaries(nanobots: Set[Nanobot]): (Int, Int, Int, Int, Int, Int) = {
    val minX = nanobots.map(_.pos.x).min
    val maxX = nanobots.map(_.pos.x).max
    val minY = nanobots.map(_.pos.y).min
    val maxY = nanobots.map(_.pos.y).max
    val minZ = nanobots.map(_.pos.z).min
    val maxZ = nanobots.map(_.pos.z).max
    (minX, maxX, minY, maxY, minZ, maxZ)
  }

  def searchRegionFromAroundPosition(pos: Position, radius: Int): Region = {
    val xs = (pos.x - radius) to (pos.x + radius)
    val ys = (pos.y - radius) to (pos.y + radius)
    val zs = (pos.z - radius) to (pos.z + radius)
    Region(xs, ys, zs)
  }

  def findBest3DArea(myPosition: Position)(nanobots: Set[Nanobot], resolution: Int, region: Region): Set[(Position, Int)] = {
    val probePoints =
      for (x <- region.xs by resolution;
           y <- region.ys by resolution;
           z <- region.zs by resolution)
        yield Position(x, y, z)

    val (_, winningRegions) =
      probePoints.par.map { pos =>
        val reachingNanobots = nanobots.filter(nano => isInRange(nano, pos))
        (pos, reachingNanobots)
      }.groupBy { case (_, reaching) => reaching.size }
        .maxBy { case (size, _) => size }
    val bestProbePoints = winningRegions.toSet.seq

    if (bestProbePoints.size == probePoints.size) {
//      if (resolution > 10) println(s"didn't find better region in resolution = $resolution")
      val closestPos = findClosestPositionTo(bestProbePoints.map(_._1))(myPosition)
      bestProbePoints.filter(_._1 == closestPos)
        .map { case (a, b) => (a, b.size) }
    }
    else if (resolution == 1) bestProbePoints.map { case (a, b) => (a, b.size) }
    else {
      val newResolution = Math.max(resolution / 2, 1)
//      println(s"Invoking recursively for ${bestProbePoints.size} regions with resolution = $newResolution")
      val s =
        bestProbePoints.flatMap { case (candidatePos, _) =>
          val searchRegion = searchRegionFromAroundPosition(candidatePos, resolution / 2)
          findBest3DArea(myPosition)(nanobots, newResolution, searchRegion)
        }.groupBy { case (_, reaching) => reaching }
          .maxBy { case (size, _) => size }
          ._2
//      println(s"Exiting recursive processing of subregions with resolution = $newResolution")
      s
    }
  }

  def findPositionsInRangeOfTheMostNanobots(myPosition: Position)(allNanobots: Set[Nanobot]): Set[Position] = {
    val minRadius = allNanobots.map(_.signalRadius).min
    val resolution = Math.max(minRadius / 30, 1)
    val (minX, maxX, minY, maxY, minZ, maxZ) = getBoundaries(allNanobots)
    val regionWithAllNanobots = Region(
      minX to maxX,
      minY to maxY,
      minZ to maxZ)
    val candidates = findBest3DArea(myPosition)(allNanobots, resolution, regionWithAllNanobots)
    candidates.map(_._1)
  }

  def findClosestPositionTo(positions: Set[Position])(targetPosition: Position): Position =
    positions.minBy(p => calcDistance(targetPosition, p))

  def solvePart2(lines: List[String]): Int = {
    val myPosition = Position(0, 0, 0)
    val nanobots = parseNanobots(lines)
    val positions = findPositionsInRangeOfTheMostNanobots(myPosition)(nanobots)
    val bestPosition = findClosestPositionTo(positions)(myPosition)
    calcDistance(myPosition, bestPosition)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day23-input")
    val solution1 = countNanobotsInRangeOfTheStrongestOne(input)
    println(solution1)

    println(new Date())
    val solution2 = solvePart2(input)
    println(solution2)
    println(new Date())
  }
}
