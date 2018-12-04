package com.lukgru.algo.advent2018.day4

import com.lukgru.algo.advent2018.utils.InputLoader

object ReposeRecord {

  case class Shift(minutesAsleep: Set[Int])

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
    val pattern = "\\[(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\].*".r
    val pattern(year, month, day, hour, minute) = line
    val isAwake = !line.contains("falls asleep")
    if (hour.toInt == 0)
      (minute.toInt, isAwake)
    else
      (0, isAwake)
  }

  def logsToShift(shiftLogs: List[String]): (Int, Shift) = {
    val guardId = parseGuardIdLine(shiftLogs.head)
    val startingMinute = parseLogLine(shiftLogs.head)._1
    val events = shiftLogs.map(parseLogLine).toMap

    var isAwake = true
    val minutes =
      for (minute <- startingMinute until 60) yield {
        isAwake = events.getOrElse(minute, isAwake)
        if (isAwake) -1
        else minute
      }
    val minutesAsleep = minutes.filter(_ >= 0).toSet
    (guardId, Shift(minutesAsleep))
  }

  def parseGuards(logLines: List[String]): List[Guard] = {
    val logsByShift = groupLogsByShift(logLines)
    val shiftsByGuard = logsByShift.map(logsToShift)
    shiftsByGuard.groupBy(_._1)
      .map { case (guardId, shiftsWithIds) => Guard(guardId, shiftsWithIds.map(_._2)) }
      .toList
  }

  def findMostSleepyGuard(guards: List[Guard]): Guard = {
    guards.maxBy(g => g.shifts.map(_.minutesAsleep.size).sum)
  }

  def findMostSleepyMinuteWithOccurrences(guard: Guard): (Int, Int) = {
    val minutesWithOccurrencesMap = guard.shifts
      .flatMap(_.minutesAsleep)
      .groupBy(identity)
      .map { case (minute, occurrences) => (minute, occurrences.length) }

    if (minutesWithOccurrencesMap.nonEmpty)
      minutesWithOccurrencesMap.maxBy(_._2)
    else
      (-1, -1)
  }

  def solvePart1(input: List[String]): (Int, Int) = {
    val guards = parseGuards(input)
    val mostSleepyGuard = findMostSleepyGuard(guards)
    val mostSleepyMinute = findMostSleepyMinuteWithOccurrences(mostSleepyGuard)
    (mostSleepyGuard.id, mostSleepyMinute._1)
  }

  def solvePart2(input: List[String]): (Int, Int) = {
    val guards = parseGuards(input)
    val (id, (minute, _)) =
      guards
        .map(g => (g.id, findMostSleepyMinuteWithOccurrences(g)))
        .maxBy { case (_, occurrences) => occurrences._2 }
    (id, minute)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day4-input")
    val solution1 = solvePart1(input)
    println(solution1._1 * solution1._2)

    val solution2 = solvePart2(input)
    println(solution2._1 * solution2._2)
  }
}
