package com.lukgru.algo.advent2018.day4

import org.scalatest.FunSuite

class ReposeRecordTest extends FunSuite {

  val logs = List(
    "[1518-11-01 00:05] falls asleep",
    "[1518-11-02 00:50] wakes up",
    "[1518-11-01 00:30] falls asleep",
    "[1518-11-05 00:55] wakes up",
    "[1518-11-03 00:05] Guard #10 begins shift",
    "[1518-11-04 00:46] wakes up",
    "[1518-11-01 00:00] Guard #10 begins shift",
    "[1518-11-04 00:02] Guard #99 begins shift",
    "[1518-11-03 00:24] falls asleep",
    "[1518-11-05 00:45] falls asleep",
    "[1518-11-01 23:58] Guard #99 begins shift",
    "[1518-11-04 00:36] falls asleep",
    "[1518-11-03 00:29] wakes up",
    "[1518-11-01 00:55] wakes up",
    "[1518-11-02 00:40] falls asleep",
    "[1518-11-05 00:03] Guard #99 begins shift",
    "[1518-11-01 00:25] wakes up",
  )

  test("should parse log line correctly") {
    //given
    val logLine = "[1518-08-06 00:45] falls asleep"

    //when
    val (minute, isAwake) = ReposeRecord.parseLogLine(logLine)

    //then
    assert(minute == 45)
    assert(!isAwake)
  }

  test("should group log lines by shift") {
    //when
    val groups = ReposeRecord.groupLogsByShift(logs)

    //then
    assert(groups.contains(List(
      "[1518-11-01 00:00] Guard #10 begins shift",
      "[1518-11-01 00:05] falls asleep",
      "[1518-11-01 00:25] wakes up",
      "[1518-11-01 00:30] falls asleep",
      "[1518-11-01 00:55] wakes up"
    )))
    assert(groups.contains(List(
      "[1518-11-01 23:58] Guard #99 begins shift",
      "[1518-11-02 00:40] falls asleep",
      "[1518-11-02 00:50] wakes up",
    )))
    assert(groups.contains(List(
      "[1518-11-03 00:05] Guard #10 begins shift",
      "[1518-11-03 00:24] falls asleep",
      "[1518-11-03 00:29] wakes up",
    )))
    assert(groups.contains(List(
      "[1518-11-04 00:02] Guard #99 begins shift",
      "[1518-11-04 00:36] falls asleep",
      "[1518-11-04 00:46] wakes up",
    )))
    assert(groups.contains(List(
      "[1518-11-05 00:03] Guard #99 begins shift",
      "[1518-11-05 00:45] falls asleep",
      "[1518-11-05 00:55] wakes up"
    )))
  }

  test("should convert shift logs to shift object") {
    //given
    val shiftLogs = List(
      "[1518-11-05 00:03] Guard #99 begins shift",
      "[1518-11-05 00:45] falls asleep",
      "[1518-11-05 00:55] wakes up"
    )

    //when
    val shift = ReposeRecord.logsToShift(shiftLogs)

    //then
    assert(shift._1 == 99)
    assert(shift._2.minutesAsleep == (45 until 55).toSet)
  }

  test("should convert shift logs to shift object when shift starts before midnight") {
    //given
    val shiftLogs = List(
      "[1518-11-04 23:50] Guard #99 begins shift",
      "[1518-11-05 00:45] falls asleep",
      "[1518-11-05 00:55] wakes up"
    )

    //when
    val shift = ReposeRecord.logsToShift(shiftLogs)

    //then
    assert(shift._1 == 99)
    assert(shift._2.minutesAsleep == (45 until 55).toSet)
  }


  test("should find most sleepy guard") {
    //given
    val guards = ReposeRecord.parseGuards(logs)

    //when
    val mostSleepyGuard = ReposeRecord.findMostSleepyGuard(guards)
    println(mostSleepyGuard)

    //then
    assert(mostSleepyGuard.id == 10)
  }

}
