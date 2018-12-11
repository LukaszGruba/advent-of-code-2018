package com.lukgru.algo.advent2018.day11

import com.lukgru.algo.advent2018.utils.Timer

object ChronalCharge {

  def powerLevel(gridSerialNumber: Int, x: Int, y: Int): Int = {
    val rackId = x + 10
    val n = rackId * (gridSerialNumber + (rackId * y))
    val hundredsDigit = (n / 100) % 10
    hundredsDigit - 5
  }

  def squarePower(squareSize: Int)(gridSerialNumber: Int)(x: Int, y: Int): Int = {
    val powers =
      for (i <- 0 until squareSize; j <- 0 until squareSize) yield powerLevel(gridSerialNumber, x + i, y + j)
    powers.sum
  }

  def findMaxPowerSquareCoords(squareSize: Int)(gridSerialNumber: Int): (Int, Int) = {
    println(s"square size = $squareSize")
    val maxSize = 300
    val squares = for (x <- 1 to maxSize - squareSize + 1; y <- 1 to maxSize - squareSize + 1) yield (x, y)
    squares.map(s => (s, squarePower(squareSize)(gridSerialNumber)(s._1, s._2)))
      .maxBy(_._2)
      ._1
  }

  def solvePart2(gridSerialNumber: Int): (Int, Int, Int, Int) = {
    (1 to 300)
      .par
      .map { squareSize =>
        val maxCoords = Timer.logTime(findMaxPowerSquareCoords(squareSize)(gridSerialNumber))
        val value = squarePower(squareSize)(gridSerialNumber)(maxCoords._1, maxCoords._2)
        (squareSize, maxCoords, value)
      }
      .map { case (size, (x, y), value) => (x, y, size, value) }
      .maxBy(_._4)
  }

  def main(args: Array[String]): Unit = {
    val gridSerialNumber = 4455
    val solution1 = findMaxPowerSquareCoords(3)(gridSerialNumber)
    println(solution1)

    val solution2 = solvePart2(gridSerialNumber)
    println(solution2)
  }

}
