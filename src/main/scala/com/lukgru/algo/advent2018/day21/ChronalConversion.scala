package com.lukgru.algo.advent2018.day21

import com.lukgru.algo.advent2018.day19.GoWithTheFlow.{executeUntilHalts, parseIpReg, parseProgram, regs}
import com.lukgru.algo.advent2018.utils.InputLoader

object ChronalConversion {

  def run(programLines: List[String]): Unit = {
    val ipReg = parseIpReg(programLines)
    val program = parseProgram(programLines)
    val initReg = regs(0, 0, 0, 0, 0, 0)
    executeUntilHalts(ipReg)(initReg)(program.toVector)
  }

  /*
  After reverse engineering the Elves program I noticed that the program halts only when condition reg0 == reg2 is met.
  So in order to solve part 1 I run the program with conditional breakpoint on the condition instruction to see what is the value of reg2 the first time this condition is checked - that's the solution.
  In order to solve part 2 I recorded all the possible halting values of reg2 in a Set until they started to repeat - the last halting value of reg2 before repetition is the solution to part 2.
   */
  def main(args: Array[String]): Unit = {
    val programLines = InputLoader.loadLines("day21-input")
    run(programLines)
  }
}
