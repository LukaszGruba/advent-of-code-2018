package com.lukgru.algo.advent2018.day25

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object FourDimentionalAdventure {

  case class Point(d1: Int, d2: Int, d3: Int, d4: Int)

  case class Constellation(points: Set[Point])

  def parsePoints(lines: List[String]): List[Point] =
    lines.map { line =>
      val pointPattern = "(-?\\d*),(-?\\d*),(-?\\d*),(-?\\d*)".r
      val pointPattern(x, y, z, t) = line
      Point(x.toInt, y.toInt, z.toInt, t.toInt)
    }

  def manhattanDistance(p1: Point, p2: Point): Int =
    Math.abs(p1.d1 - p2.d1) + Math.abs(p1.d2 - p2.d2) + Math.abs(p1.d3 - p2.d3) + Math.abs(p1.d4 - p2.d4)

  def areInTheSameConstellation(p1: Point, p2: Point): Boolean =
    manhattanDistance(p1, p2) <= 3

  def isInConstellation(p: Point, constellation: Constellation): Boolean =
    constellation.points.exists(areInTheSameConstellation(p, _))

  def joinConstellations(constellations: Set[Constellation]): Constellation =
    Constellation(constellations.flatMap(_.points))

  def addToConstellation(point: Point, constellation: Constellation): Constellation =
    constellation.copy(points = constellation.points + point)

  def findConstellations(points: List[Point]): Set[Constellation] = {
    @tailrec
    def findConstellationsRec(points: List[Point], constellations: Set[Constellation]): Set[Constellation] =
      points match {
        case Nil => constellations
        case point +: rest =>
          val matchingConstellations = constellations.filter(isInConstellation(point, _))
          matchingConstellations match {
            case empty if empty.isEmpty => findConstellationsRec(rest, constellations + Constellation(Set(point)))
            case _ =>
              val joinedMatching = joinConstellations(matchingConstellations)
              val constellationWithPoint = addToConstellation(point, joinedMatching)
              val allConstellationsExceptJoined = constellations.diff(matchingConstellations)
              findConstellationsRec(rest, allConstellationsExceptJoined + constellationWithPoint)
          }
      }

    findConstellationsRec(points, Set.empty)
  }

  def solvePart1(lines: List[String]): Int = {
    val points = parsePoints(lines)
    val constellations = findConstellations(points)
    constellations.size
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day25-input")
    val solution1 = solvePart1(input)
    println(solution1)
  }

}
