package com.lukgru.algo.advent2018.day11

object ChronalCharge {

  def powerLevel(gridSerialNumber: Int)(x: Int, y: Int): Int = {
    val rackId = x + 10
    val n = rackId * (gridSerialNumber + (rackId * y))
    val hundredsDigit = (n / 100) % 10
    hundredsDigit - 5
  }

  def squarePower(squareSize: Int)(gridSerialNumber: Int)(x: Int, y: Int): Int = {
    val powerFun = powerLevel(gridSerialNumber) _
    val powers =
      for (i <- 0 until squareSize; j <- 0 until squareSize) yield powerFun(x + i, y + j)
    powers.sum
  }

  def findMaxPowerSquareCoords(squareSize: Int)(gridSerialNumber: Int): (Int, Int) = {
    val maxSize = 300
    val squares = for (x <- 1 to maxSize - 2; y <- 1 to maxSize - 2) yield (x, y)
    squares.map(s => (s, squarePower(squareSize)(gridSerialNumber)(s._1, s._2)))
      .maxBy(_._2)
      ._1
  }

  def main(args: Array[String]): Unit = {
    val gridSerialNumber = 4455
    val solution1 = findMaxPowerSquareCoords(3)(gridSerialNumber)
    println(solution1)
  }

}
