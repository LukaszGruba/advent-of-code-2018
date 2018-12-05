package com.lukgru.algo.advent2018.day5

import com.lukgru.algo.advent2018.utils.InputLoader

object AlchemicalReduction {

  def isReacting(c1: Char, c2: Char): Boolean = {
    c1.toLower == c2.toLower && (c1.isLower ^ c2.isLower)
  }

  def reduce(s: String): String = {
    s.foldLeft("") {
      case (acc, c) =>
        acc.lastOption
          .map(last =>
            if (isReacting(last, c))
              acc.init
            else
              acc + c
          )
          .getOrElse(c.toString)
    }
  }

  def removeUnit(polymer: String)(unit: Char): String =
    polymer.replace(unit.toString, "").replace(unit.toUpper.toString, "")

  def optimizedReduce(polymer: String): String = {
    def opt: Char => String = removeUnit(polymer)
    ('a' to 'z').map(opt)
      .map(reduce)
      .minBy(_.length)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day5-input").head
    val solution1 = reduce(input)
    println(solution1.length)

    val solution2 = optimizedReduce(input)
    println(solution2.length)
  }
}
