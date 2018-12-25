package com.lukgru.algo.advent2018.day23

import com.lukgru.algo.advent2018.utils.InputLoader

object ExperimentalEmergencyTeleportation {

  case class Position(x: Int, y: Int, z: Int)

  case class Nanobot(pos: Position, signalRadius: Int)

  def parseNanobots(lines: List[String]): Set[Nanobot] = {
    val nanobotPattern = "pos=<(-?\\d*),(-?\\d*),(-?\\d*)>, r=(-?\\d*)".r
    lines.map { line =>
      val nanobotPattern(x, y, z, r) = line
      Nanobot(Position(x.toInt, y.toInt, z.toInt), r.toInt)
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

  def solvePart1(lines: List[String]): Int = {
    val nanobots = parseNanobots(lines)
    val strongest = findTheStrongestNanobot(nanobots)
    val inRangeOfTheStrongest = getAllInRangeOf(nanobots)(strongest)
    inRangeOfTheStrongest.size
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day23-input")
    val solution1 = solvePart1(input)
    println(solution1)
  }
}
