package com.lukgru.algo.advent2018.day12

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object SubterraneanSustainability {

  case class Pot(no: Int, plantPresent: Boolean)

  case class Rule(pattern: List[Boolean], value: Boolean)

  def formatGeneration(generation: List[Pot]): String = {
    generation.map(_.plantPresent)
      .map {
        case true => '#'
        case false => '.'
      }.mkString
  }

  def parseState(state: String): List[Pot] =
    state.replace("initial state: ", "")
      .zipWithIndex
      .map {
        case ('#', i) => Pot(i, plantPresent = true)
        case ('.', i) => Pot(i, plantPresent = false)
      }.toList

  def parseRule(s: String): Rule = {
    val pattern = "([\\.,\\#]+) => ([\\.,#])".r
    val pattern(p, v) = s
    Rule(parseState(p).map(_.plantPresent), parseState(v).map(_.plantPresent).head)
  }

  def trimGeneration(generation: List[Pot]): List[Pot] = {
    val trimmedFromBeginning = generation.dropWhile(p => !p.plantPresent)
    val firstIndex = trimmedFromBeginning.head.no
    val prefix = (firstIndex - 4 until firstIndex).map(i => Pot(i, plantPresent = false)).toList
    val trimmed = trimmedFromBeginning.reverse.dropWhile(p => !p.plantPresent).reverse
    val lastIndex = trimmed.last.no
    val suffix = ((lastIndex + 1) to (lastIndex + 4)).map(i => Pot(i, plantPresent = false)).toList
    prefix ++ trimmed ++ suffix
  }

  def growGeneration(rules: List[Rule])(generation: List[Pot]): List[Pot] = {
    @tailrec
    def grow(current: List[Pot], result: List[Pot]): List[Pot] = {
      if (current.length < 5) result
      else {
        val pattern = current.take(5).map(_.plantPresent)
        val newValue = rules
          .find(r => r.pattern == pattern)
          .exists(r => r.value)
        grow(current.tail, result :+ Pot(current(2).no, newValue))
      }
    }

    grow(trimGeneration(generation), List.empty)
  }

  def parseRules(rules: List[String]): List[Rule] = rules.map(parseRule)

  def calcScore(pots: List[Pot]): Int =
    pots.filter(_.plantPresent)
      .map(_.no)
      .sum

  def solvePart1(numberOfGenerations: Int)(lines: List[String]): Int = {
    val initState = parseState(lines.head)
    val rules = parseRules(lines.drop(2))
    val growFunc = growGeneration(rules) _
    val finalGen = (1 to numberOfGenerations).foldLeft(initState) {
      case (prevState, _) => growFunc(prevState)
    }
    calcScore(finalGen)
  }

  def findStableState(lines: List[String]): Int = {
    val initState = parseState(lines.head)
    val rules = parseRules(lines.drop(2))
    val growFunc = growGeneration(rules) _

    var isCycleFound = false
    var i = 0
    var gens = Set.empty[String]
    var gen = initState
    while (!isCycleFound && i < 50000000000L) {
      gen = growFunc(gen)
      val str = formatGeneration(gen)
      isCycleFound = gens.contains(str)
      gens = gens + str
      i += 1
    }
    println(i)
    println(gen)
    i
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day12-input")
    val solution1 = solvePart1(20)(input)
    println(solution1)

    //FIXME: solution for 50.000.000.000 gen was calculated manually when stable state was found and score interval between stable gens was found. This should be automated
    val solution2 = findStableState(input)
    println(solution2)
  }

}
