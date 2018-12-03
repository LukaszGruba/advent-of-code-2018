package com.lukgru.algo.advent2018.day3

import com.lukgru.algo.advent2018.utils.InputLoader

object NoMatterHowYouSliceIt {

  case class Rectangle(id: Int, x: Int, y: Int, width: Int, height: Int)

  def parseRectangle(str: String): Rectangle = {
    val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r
    val pattern(id, x, y, width, height) = str
    Rectangle(id.toInt, x.toInt, y.toInt, width.toInt, height.toInt)
  }

  def parseRectangles(lines: List[String]): List[Rectangle] = lines.map(parseRectangle)

  def areaPoints(rect: Rectangle) = {
    for (x <- rect.x until rect.x + rect.width;
         y <- rect.y until rect.y + rect.height)
      yield (x, y)
  }
  
  def areaPointsList(rectangles: List[Rectangle]): List[(Int, Int)] = {
    rectangles.foldLeft(List.empty[(Int,Int)]) { case (points, rectangle) => points ++ areaPoints(rectangle) }
  }

  def calcOverlapArea(rectangles: List[Rectangle]): Int = {
    val allPoints = areaPointsList(rectangles)
    val duplicates = allPoints.groupBy(identity)
        .map { case (k, list) => (k, list.size)}
        .filter { case (_, size) => size > 1}
    duplicates.size
  }

  def solvePart1(): Unit = {
    val input = InputLoader.loadLines("day3-input")
    val rectangles = parseRectangles(input)
    val overlapArea = calcOverlapArea(rectangles)
    println(overlapArea)
  }

  def main(args: Array[String]): Unit = {
    solvePart1()
  }
}
