package com.lukgru.algo.advent2018.day17

import com.lukgru.algo.advent2018.day17.ReservoirResearch.WaterState.WaterState
import com.lukgru.algo.advent2018.utils.InputLoader

object ReservoirResearch {

  object WaterState extends Enumeration {
    type WaterState = Value
    val Flowing, Still = Value
  }

  case class Position(x: Int, y: Int)

  case class Water(pos: Position, state: WaterState)

  def parseClay(lines: List[String]): Set[Position] = {
    def parseLine(line: String): Set[Position] = {
      val xyStrings = line.split(", ").sortBy(_.head)
      val xString = xyStrings.head
      val yString = xyStrings(1)
      val singlePattern = "^[x,y]=(\\d*)$".r
      val rangePattern = "^[x,y]=(\\d*)\\.\\.(\\d*)$".r
      val (xs, ys) =
        if (singlePattern.findFirstIn(xString).isDefined) {
          val singlePattern(x) = xString
          val rangePattern(yFrom, yTo) = yString
          (List(x.toInt), (yFrom.toInt to yTo.toInt).toList)
        } else {
          val rangePattern(xFrom, xTo) = xString
          val singlePattern(y) = yString
          ((xFrom.toInt to xTo.toInt).toList, List(y.toInt))
        }
      val clay = for (x <- xs; y <- ys) yield Position(x, y)
      clay.toSet
    }

    lines.flatMap(parseLine).toSet
  }

  def printClay(clay: Set[Position]): Unit = {
    val xMin = clay.map(_.x).min
    val xMax = clay.map(_.x).max
    val yMin = clay.map(_.y).min
    val yMax = clay.map(_.y).max
    val sb = new StringBuilder()
    for (y <- yMin to yMax) {
      for (x <- xMin to xMax) {
        if (clay.contains(Position(x,y))) sb.append('#')
        else sb.append('.')
      }
      sb.append('\n')
    }
    println(sb.mkString)
  }

  def solvePart1(lines: List[String]): Int = {
    val clay = parseClay(lines)
    printClay(clay)
    ???
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day17-input")
    val numberOfReachedTiles = solvePart1(input)
    println(numberOfReachedTiles)
  }

}
