package com.lukgru.algo.advent2018.day4

import com.lukgru.algo.advent2018.utils.InputLoader

object ReposeRecord {

  case class Shift(minutesAwake: Set[Int])

  case class Guard(id: Int, shifts: List[Shift])

  def groupLogsByShift(logLines: List[String]): List[List[String]] = {
    val (shifts, lastShift) = logLines.sorted
      .foldLeft((List.empty[List[String]], List.empty[String])) {
        case ((shifts: List[List[String]], lastShift: List[String]), line: String) =>
          if (line.contains("begins shift")) {
            (lastShift :: shifts, List(line))
          } else {
            (shifts, lastShift :+ line)
          }
      }
    (lastShift :: shifts).filter(_.nonEmpty)
  }

  def parseGuardIdLine(line: String): Int = {
    val pattern = ".*Guard #(\\d*) begins shift".r
    val pattern(id) = line
    id.toInt
  }

  def parseLogLine(line: String): (Int, Boolean) = {
    val pattern = "\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:(\\d{2})\\].*".r
    val pattern(minute) = line
    val fallsAsleep = line.contains("falls asleep")
    (minute.toInt, fallsAsleep)
  }

  def logsToShift(logs: List[String]): (Int, Shift) = {
    val guardId = parseGuardIdLine(logs.head)
    val events = logs.map(parseLogLine).toMap

    var isAsleep = false
    val minutes =
      for (minute <- 0 until 60) yield {
        isAsleep = events.getOrElse(minute, isAsleep)
        if (isAsleep) -1
        else minute
      }
    val minutesAwake = minutes.filter(_ >= 0).toSet
    (guardId, Shift(minutesAwake))
  }

  def parseGuards(logLines: List[String]): List[Guard] = {
    val logsByShift = groupLogsByShift(logLines)
    val shiftsByGuard = logsByShift.map(logsToShift)
    shiftsByGuard.groupBy(_._1)
      .map { case (guardId, shiftsWithIds) => Guard(guardId, shiftsWithIds.map(_._2))}
      .toList
  }

  def findMostSleepyGuard(guards: List[Guard]): Guard = {
    guards.minBy(g => g.shifts.map(_.minutesAwake.size).sum)
  }

  def findMostSleepyMinute(guard: Guard): Int = {
    guard.shifts
      .flatMap(_.minutesAwake)
      .groupBy(identity)
      .maxBy { case (_, occurrences) => occurrences.length }
      ._1
  }

  def solvePart1(input: List[String]): (Int, Int) = {
    val guards = parseGuards(input)
    val mostSleepyGuard = findMostSleepyGuard(guards)
    val mostSleepyMinute = findMostSleepyMinute(mostSleepyGuard)
    (mostSleepyGuard.id, mostSleepyMinute)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day4-input")
    val solution = solvePart1(input)
    println(solution)
  }
}
