package com.lukgru.algo.advent2018.day14

import scala.annotation.tailrec

object ChocolateCharts {

  type Scoreboard = Vector[Int]

  case class Elf(position: Int)

  def combineRecipes(r1: Int, r2: Int): Vector[Int] = {
    val sum = r1 + r2
    if (sum >= 10) {
      val s1 = sum / 10
      val s2 = sum % 10
      Vector(s1, s2)
    }
    else Vector(sum)
  }

  def getNAfterMRecipes(n: Int)(m: Int)(scoreboard: Scoreboard): Vector[Int] = scoreboard.slice(m, m + n)

  def moveElf(elf: Elf, scoreboard: Scoreboard): Elf = {
    val currentScore = scoreboard(elf.position)
    val newPosition = (elf.position + currentScore + 1) % scoreboard.length
    elf.copy(position = newPosition)
  }

  def getCurrentRecipes(scoreboard: Scoreboard, elves: Vector[Elf]): (Int, Int) = (scoreboard(elves.head.position), scoreboard(elves(1).position))

  @tailrec
  def evolveScoreboard(scoreboard: Scoreboard, elves: Vector[Elf], targetLength: Int): Scoreboard = {
    if (scoreboard.length >= targetLength) scoreboard
    else {
      val (r1, r2) = getCurrentRecipes(scoreboard, elves)
      val newRecipes = combineRecipes(r1, r2)
      val newScoreboard = scoreboard ++ newRecipes
      val newElves = elves.map(e => moveElf(e, newScoreboard))
      evolveScoreboard(newScoreboard, newElves, targetLength)
    }
  }

  def findBestNAfterM(n: Int, initElves: Vector[Elf], initScoreboard: Scoreboard)(m: Int): Vector[Int] = {
    val getBestNAfterM = getNAfterMRecipes(n)(m) _
    val targetScoreboard = evolveScoreboard(initScoreboard, initElves, n + m)
    getBestNAfterM(targetScoreboard)
  }

  def formatSolution(s: Vector[Int]): String = s.mkString

  def main(args: Array[String]): Unit = {
    val input = 47801
    val solution1 = findBestNAfterM(10, Vector(Elf(0), Elf(1)), Vector(3, 7))(input)
    println(formatSolution(solution1))
  }

}
