package com.lukgru.algo.advent2018.day17

import com.lukgru.algo.advent2018.day17.ReservoirResearch.WaterState.WaterState
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.collection.parallel.ParMap

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

  def printState(water: ParMap[Position, Water], clay: Set[Position]): Unit = {
    val allPositions = water.map(_._2.pos) ++ clay
    val xMin = allPositions.map(_.x).min
    val xMax = allPositions.map(_.x).max
    val yMin = allPositions.map(_.y).min
    val yMax = allPositions.map(_.y).max
    val sb = new StringBuilder()
    for (y <- yMin to yMax) {
      for (x <- xMin to xMax) {
        val p = Position(x, y)
        if (clay.contains(p)) sb.append('#')
        else if (water.contains(p)) {
          val w = water(p)
          val waterSign = w.state match {
            case WaterState.Flowing => '|'
            case WaterState.Still => '~'
          }
          sb.append(waterSign)
        }
        else sb.append('.')
      }
      sb.append('\n')
    }
    println(sb.mkString)
  }

  private def getPosBelow(p: Position): Position = p.copy(y = p.y + 1)

  def isEndOfStream(w: Water, allTheWater: ParMap[Position, Water], clay: Set[Position]): Boolean = {
    val posBelow = getPosBelow(w.pos)
    w.state == WaterState.Flowing && !allTheWater.contains(posBelow) && !clay.contains(posBelow)
  }

  def isInTheStream(w: Water, water: ParMap[Position, Water], clay: Set[Position]): Boolean =
    w.state == WaterState.Flowing && water.get(getPosBelow(w.pos)).exists(_.state == WaterState.Flowing)

  def didHitStillElement(w: Water, water: ParMap[Position, Water], clay: Set[Position]): Boolean = {
    val posBelow = getPosBelow(w.pos)
    w.state == WaterState.Flowing && (water.get(posBelow).exists(_.state == WaterState.Still) || clay.contains(posBelow))
  }

  def expandWater(w: Water, water: ParMap[Position, Water], clay: Set[Position]): Set[Water] = {
    def restrictedFromLeft(ws: Stream[Water]): Boolean = {
      val leftMost = ws.map(_.pos).minBy(_.x)
      clay.contains(leftMost.copy(x = leftMost.x - 1))
    }

    def restrictedFromRight(ws: Stream[Water]): Boolean = {
      val rightMost = ws.map(_.pos).maxBy(_.x)
      clay.contains(rightMost.copy(x = rightMost.x + 1))
    }

    def restrictedFromBothSides(ws: Stream[Water]): Boolean = {
      restrictedFromLeft(ws) && restrictedFromRight(ws)
    }

    val waterToTheRight =
      Stream.from(0)
        .map(x => Position(w.pos.x + x, w.pos.y))
        .takeWhile { p =>
          !clay.contains(p) && (water.get(getPosBelow(p)).exists(_.state == WaterState.Still) || clay.contains(getPosBelow(p)))
        }
        .map(p => Water(p, WaterState.Flowing))
    val waterToTheLeft =
      Stream.from(0)
        .map(x => Position(w.pos.x - x, w.pos.y))
        .takeWhile { p =>
          !clay.contains(p) && (water.get(getPosBelow(p)).exists(_.state == WaterState.Still) || clay.contains(getPosBelow(p)))
        }
        .map(p => Water(p, WaterState.Flowing))
    val combined = waterToTheLeft ++ waterToTheRight
    if (restrictedFromBothSides(combined)) {
      combined.map(_.copy(state = WaterState.Still)).toSet
    }
    else {
      val additionalOnLeft =
        if (!restrictedFromLeft(combined)) {
          val leftMost = combined.map(_.pos).minBy(_.x)
          Some(Water(leftMost.copy(x = leftMost.x - 1), WaterState.Flowing))
        } else None
      val additionalOnRight =
        if (!restrictedFromRight(combined)) {
          val rightMost = combined.map(_.pos).maxBy(_.x)
          Some(Water(rightMost.copy(x = rightMost.x + 1), WaterState.Flowing))
        } else None
      (Stream(additionalOnLeft, additionalOnRight).flatten ++ combined).toSet
    }
  }

  def tick(water: ParMap[Position, Water], clay: Set[Position]): ParMap[Position, Water] =
    water.flatMap { case (p, w) =>
      w.state match {
        case WaterState.Still => List((p, w))
        case WaterState.Flowing =>
          if (isEndOfStream(w, water, clay)) {
            val below = getPosBelow(p)
            List((p, w), (below, w.copy(pos = below)))
          } else if (isInTheStream(w, water, clay)) {
            List((p, w))
          } else if (didHitStillElement(w, water, clay)) {
            expandWater(w, water, clay).map(newW => (newW.pos, newW)).toList
          } else List.empty
      }
    }

  def solve(springX: Int)(lines: List[String]): (Int, Int) = {
    val clay = parseClay(lines)
    var water = ParMap(Position(springX, 0) -> Water(Position(springX, 0), WaterState.Flowing))
    var i = 0
    var lastWaterSize = 0
    val top = clay.map(_.y).min
    val bottom = clay.map(_.y).max
    while (lastWaterSize != water.size) {
      lastWaterSize = water.size
      water = tick(water, clay).filter { case (p, _) => p.y <= bottom }
      i += 1
      println(i)
    }
    printState(water, clay)
    (water.count { case (p, _) => p.y >= top }, water.count { case (_, w) => w.state == WaterState.Still })
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day17-input")
    val (numberOfReachedTiles, numberOfStillWater) = solve(500)(input)
    println(numberOfReachedTiles)
    println(numberOfStillWater)
  }

}
