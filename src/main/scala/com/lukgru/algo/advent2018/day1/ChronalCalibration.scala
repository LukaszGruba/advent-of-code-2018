package com.lukgru.algo.advent2018.day1

import com.lukgru.algo.advent2018.utils.InputLoader._

import scala.annotation.tailrec

object ChronalCalibration {

  def parseOperation(strOp: String): Int => Int = {
    val op = strOp.head
    val num = strOp.tail.toInt
    if (op == '+') {
      num.+
    } else {
      (-num).+
    }
  }

  def parseOperations(opsString: String): List[Int => Int] = {
    opsString.lines
      .map(parseOperation)
      .toList
  }

  def solvePart1() = {
    val startingFreq = 0
    val input = loadLines("day1-input")

    val ops = parseOperations(input)
    val result = ops.foldLeft(startingFreq) { case (res, f) => f(res) }
    println(result)
  }

  def solvePart2() = {
    val startingFreq = 0
    val input = loadLines("day1-input")

    val ops = Stream.continually(parseOperations(input)).flatten

    @tailrec
    def findRepeatFreq(freq: Int, ops: Stream[Int => Int], visited: Set[Int]): Int = {
      if (visited.contains(freq)) {
        freq
      } else {
        findRepeatFreq(ops.head(freq), ops.tail, visited + freq)
      }
    }

    val solution = findRepeatFreq(startingFreq, ops, Set.empty)
    println(solution)
  }

  def main(args: Array[String]): Unit = {
    solvePart1()
    solvePart2()
  }
}
