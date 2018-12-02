package com.lukgru.algo.advent2018.day2

import com.lukgru.algo.advent2018.utils.InputLoader._

object InventoryManagementSystem {

  def containsDuplicates(n: Int)(id: String): Boolean = {
    id.groupBy(identity)
      .map { case (_, occurrences) => occurrences.length }
      .exists(n.==)
  }

  def calcChecksum(ids: List[String]): Int = {
    val numberOf2s = ids.count(containsDuplicates(2))
    val numberOf3s = ids.count(containsDuplicates(3))
    numberOf2s * numberOf3s
  }

  def findSimilarIds(strings: List[String]): (String, String) = {
    val sortedWords = strings.sorted
    ("" :: sortedWords).zip(sortedWords)
      .find { case (str1, str2) => differentLetters(str1, str2).length == 1 }
      .get
  }

  def differentLetters(str1: String, str2: String): String = {
    str1.zip(str2)
      .filterNot { case (c1, c2) => c1 == c2 }
      .map(_._1)
      .mkString
  }

  def commonLetters(str1: String, str2: String): String = {
    str1.zip(str2)
      .filter { case (c1, c2) => c1 == c2 }
      .map(_._1)
      .mkString
  }

  def solvePart1(): Unit = {
    val inputLines = loadLines("day2-input")
    val solution = calcChecksum(inputLines)
    println(solution)
  }

  def solvePart2(): Unit = {
    val inputLines = loadLines("day2-input")
    val (id1, id2) = findSimilarIds(inputLines)
    val solution = commonLetters(id1, id2)
    println(solution)
  }

  def main(args: Array[String]): Unit = {
    solvePart1()
    solvePart2()
  }
}
