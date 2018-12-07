package com.lukgru.algo.advent2018.day6

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.collection.mutable

object ChronalCoordinates {

  case class Coordinate(x: Int, y: Int, name: String)

  def parseLine(line: String): (Int, Int) = {
    val pattern = "(\\d*), (\\d*)".r
    val pattern(x, y) = line
    (x.toInt, y.toInt)
  }

  def calcDistance(p1: (Int, Int), p2: (Int, Int)): Int = Math.abs(p1._1 - p2._1) + Math.abs(p1._2 - p2._2)

  //  def constructFx(a: Coordinate)(b: Coordinate): Int => Int = {
  //    val c = (a.y - b.y) / (a.x - b.x)
  //    x => c * x + (a.y - (c * a.x))
  //  }

  def allPointsOnSameSide(f: Int => Int, points: List[Coordinate]): Boolean = {
    val pointsNotOnLine = points.filter(p => f(p.x) != p.y)
    val isFirstPointBelowLine = f(pointsNotOnLine.head.x) > pointsNotOnLine.head.y
    pointsNotOnLine.tail
      .forall(p => (f(p.x) > p.y) == isFirstPointBelowLine)
  }

  //  def isPartOfBoundary(all: List[Coordinate])(a: Coordinate): Boolean = {
  //    val otherCoords = all.filterNot(a.==)
  //    val fxa = constructFx(a) _
  //    val lineFs = otherCoords.map(fxa)
  //    lineFs.exists(f => allPointsOnSameSide(f, all))
  //  }

  //  def constructBoundary(all: List[Coordinate]): List[Coordinate] = {
  //    all.filter(p => isPartOfBoundary(all.filter(p.==))(p))
  //  }

  def constructBoundary(all: List[Coordinate], mapBoundary: (Int, Int, Int, Int)): List[Coordinate] = {
    val p1 =
      for (x <- mapBoundary._1 to mapBoundary._2; y <- List(mapBoundary._3, mapBoundary._4)) yield {
        findNearestCoord(all, x, y)
      }
    val p2 =
      for (x <- List(mapBoundary._1, mapBoundary._2); y <- mapBoundary._3 to mapBoundary._4) yield {
        findNearestCoord(all, x, y)
      }
    (p1 ++ p2).flatten.distinct.toList
  }

  def calcMapBoundary(coordinates: List[Coordinate]): (Int, Int, Int, Int) = {
    val minX = coordinates.minBy(_.x).x
    val maxX = coordinates.maxBy(_.x).x
    val minY = coordinates.minBy(_.y).y
    val maxY = coordinates.maxBy(_.y).y
    (minX, maxX, minY, maxY)
  }

  def findNearestCoord(coordinates: List[Coordinate], x: Int, y: Int): Option[Coordinate] = {
    val min = coordinates.minBy(c => calcDistance((c.x, c.y), (x, y)))
    if (coordinates.map(c => calcDistance((c.x, c.y), (x, y))).count(size => size == calcDistance((min.x, min.y), (x, y))) > 1) {
      None
    } else {
      Some(min)
    }
  }

  def solvePart1(input: List[String]): Int = {
    val coords =
      input.zipWithIndex
        .map { case (line, name) =>
          val (x, y) = parseLine(line)
          Coordinate(x, y, name.toString)
        }
    val mapBoundary = calcMapBoundary(coords)
    val boundary = constructBoundary(coords, mapBoundary)
    val areaCounters: mutable.Map[String, Int] = mutable.Map.empty
    for (x <- mapBoundary._1 to mapBoundary._2;
         y <- mapBoundary._3 to mapBoundary._4) {
      val nearestCoord = findNearestCoord(coords, x, y)
      nearestCoord.foreach(nearest => areaCounters.put(nearest.name, areaCounters.getOrElse(nearest.name, 0) + 1))
    }
    areaCounters.filterNot(e => boundary.exists(c => c.name == e._1))
      .maxBy(e => e._2)
      ._2
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day6-input")
    val solution = solvePart1(input)
    println(solution)
  }

}
