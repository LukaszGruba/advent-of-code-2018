package com.lukgru.algo.advent2018.day18

import com.lukgru.algo.advent2018.day18.SettlersOfTheNorthPole.State.State
import com.lukgru.algo.advent2018.utils.InputLoader

object SettlersOfTheNorthPole {

  val printingEnabled: Boolean = false

  object State extends Enumeration {
    type State = Value
    val Ground, Tree, Lumberyard = Value
  }

  case class Position(x: Int, y: Int)

  def parseState(s: Char): State = s match {
    case '.' => State.Ground
    case '|' => State.Tree
    case '#' => State.Lumberyard
  }

  def parseInput(lines: List[String]): Map[Position, State] =
    lines.map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (line, y) => line.map { case (c, x) => (Position(x, y), parseState(c)) } }
      .toMap

  def printMap(map: Map[Position, State]): Unit = {
    if (printingEnabled) {
      def formatState(s: State): Char = s match {
        case State.Ground => '.'
        case State.Tree => '|'
        case State.Lumberyard => '#'
      }

      val xMax = map.keys.map(_.x).max
      val yMax = map.keys.map(_.y).max
      val sb = new StringBuilder
      for (y <- 0 to yMax) {
        for (x <- 0 to xMax) {
          val state = map(Position(x, y))
          sb.append(formatState(state))
        }
        sb.append('\n')
      }
      println(sb.mkString)
    }
  }

  def evolveAreaState(state: State, surroundings: List[State]): State = state match {
    case State.Ground => if (surroundings.count(State.Tree.==) >= 3) State.Tree else State.Ground
    case State.Tree => if (surroundings.count(State.Lumberyard.==) >= 3) State.Lumberyard else State.Tree
    case State.Lumberyard => if (surroundings.contains(State.Lumberyard) && surroundings.contains(State.Tree)) State.Lumberyard else State.Ground
  }

  def evolve(map: Map[Position, State]): Map[Position, State] = {
    def getSurroundings(p: Position): List[State] = {
      val surroundings =
        for (i <- -1 to 1; j <- -1 to 1) yield {
          if (i == 0 && j == 0) None
          else map.get(Position(p.x + i, p.y + j))
        }
      surroundings.flatten.filterNot(p.==).toList
    }

    map.map { case (pos, state) =>
      val surr = getSurroundings(pos)
      val newState = evolveAreaState(state, surr)
      (pos, newState)
    }
  }

  def findValueCycle(map: Map[Position, State]): Int = ???

  def calcValue(state: Map[Position, State]): Int = {
    val woodenAcres = state.values.count(State.Tree.==)
    val lumberyards = state.values.count(State.Lumberyard.==)
    woodenAcres * lumberyards
  }

  def solveAfterNMinutes(n: Int)(lines: List[String]): Int = {
    val initState = parseInput(lines)
    val finalState =
      (1 to n).foldLeft(initState) {
        (prevState, i) => {
          val newState = evolve(prevState)
          printMap(newState)
          println(s"i: $i\tvalue: ${calcValue(newState)}")
          newState
        }
      }
    calcValue(finalState)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day18-input")
    val solution1 = solveAfterNMinutes(10)(input)
    println(solution1)

    val solution2 = solveAfterNMinutes(1000000000)(input)
    println(solution2)
  }

}
