package com.lukgru.algo.advent2018.day2

import com.lukgru.algo.advent2018.utils.InputLoader._

object InventoryManagementSystem {

  def containsDuplicates(n: Int)(id: String) = {
    id.groupBy(identity)
      .map { case (_, occurrences) => occurrences.length}
      .exists(n.==)
  }

  def calcChecksum(ids: List[String]): Int = {
    val numberOf2s = ids.count(containsDuplicates(2))
    val numberOf3s = ids.count(containsDuplicates(3))
    println(s"2s=$numberOf2s 3s=$numberOf3s")
    numberOf2s * numberOf3s
  }

  def solvePart1(): Unit = {
    val inputLines = loadLines("day2-input")
    val solution = calcChecksum(inputLines)
    println(solution)
  }

  def main(args: Array[String]): Unit = {
    solvePart1()
  }
}
