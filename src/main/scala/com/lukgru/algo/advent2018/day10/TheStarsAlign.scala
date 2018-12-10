package com.lukgru.algo.advent2018.day10

import com.lukgru.algo.advent2018.utils.InputLoader

object TheStarsAlign {

  case class Point(x: Int, y: Int, vx: Int, vy: Int)

  def parsePoint(line: String): Point = {
    val pattern = "position=< ?(-?\\d*),  ?(-?\\d*)> velocity=< ?(-?\\d*),  ?(-?\\d*)>".r
    val pattern(x, y, vx, vy) = line
    Point(x.toInt, y.toInt, vx.toInt, vy.toInt)
  }

  def movePoint(p: Point): Point = p.copy(x = p.x + p.vx, y = p.y + p.vy)

  def move(points: List[Point]): List[Point] = points.map(movePoint)

  def coversReasonableArea(width: Int, height: Int)(points: List[Point]): Boolean = {
    val (minX, maxX, minY, maxY) = getBoundary(points)
    Math.abs(maxX - minX) <= width && Math.abs(maxY - minY) <= height
  }

  def calcEntropy(points: List[Point]): Int = ???

  def getBoundary(points: List[Point]): (Int, Int, Int, Int) = {
    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    val minY = points.map(_.y).min
    val maxY = points.map(_.y).max
    (minX, maxX, minY, maxY)
  }

  def printPoints(points: List[Point]): String = {
    val (minX, maxX, _, _) = getBoundary(points)
    points.groupBy(_.y)
      .toList
      .sortBy(_._1)
      .map(_._2)
      .map(line => line.sortBy(_.x))
      .map(line => {
        val xs = line.map(p => p.x).toSet
        val str =
          for (i <- minX to maxX) yield {
            if (xs.contains(i)) '#'
            else '.'
          }
        str.mkString
      })
      .mkString("\n")
  }

  //TODO: in order to solve this properly, use calculating entropy to find state with minimal entropy -> that must be when points form a text
  //HINT: entropy hits minimum when it was decreasing and suddenly starts increasing
  def solvePart1(lines: List[String]): String = {
    var points = lines.map(parsePoint)
    for (i <- 0 to 100000) {
      if (coversReasonableArea(200, 15)(points)) {
        println(i)
        println(printPoints(points))
      }
      points = move(points)
    }
    ""
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day10-input")
    val solution1 = solvePart1(input)
    println(solution1)
  }

}
