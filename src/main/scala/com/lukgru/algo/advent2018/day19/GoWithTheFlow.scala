package com.lukgru.algo.advent2018.day19

import com.lukgru.algo.advent2018.day16.ChronalClassification._
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object GoWithTheFlow {

  case class ExecutionUnit(opcode: String, args: (Int, Int, Int))

  def parseIpReg(lines: List[String]): Int = {
    val ipRegPattern = "#ip (\\d*)".r
    lines.find(_.matches(ipRegPattern.regex))
      .map { line =>
        val ipRegPattern(ipReg) = line
        ipReg.toInt
      }.get
  }

  def parseProgram(program: List[String]): List[ExecutionUnit] = {
    val pattern = "([a-z]*) (\\d*) (\\d*) (\\d*)".r
    program.filter(_.matches(pattern.regex))
      .map { line =>
        val pattern(opcode, a, b, c) = line
        ExecutionUnit(opcode, (a.toInt, b.toInt, c.toInt))
      }
  }

  def regs(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int): List[Register] = {
    List(Register(0, a), Register(1, b), Register(2, c), Register(3, d), Register(4, e), Register(5, f))
  }

  //TODO: pass instructions as parameter
  def execute(execUnit: ExecutionUnit, regs: List[Register]): List[Register] = {
    val op = allInstructions.find(i => i.opcode == execUnit.opcode).map(_.op).get
    val args = execUnit.args
    op(args._1, args._2, args._3, regs)
  }

  object Counter {
    var i: Long = 0
    def ++() = {
      i += 1
    }
  }

  @tailrec
  def execute(ipReg: Int, currentRegs: List[Register], program: Vector[ExecutionUnit]): List[Register] = {
    val ip = currentRegs(ipReg).value
    if (ip < 0 || ip >= program.length) currentRegs.updated(ipReg, Register(ipReg, currentRegs(ipReg).value - 1))
    else {
      val execUnit = program(ip.toInt)
      val regsAfterOp = execute(execUnit, currentRegs)
      val newIpValue = regsAfterOp(ipReg).value + 1
      val newRegs = regsAfterOp.updated(ipReg, Register(ipReg, newIpValue))
      if (Counter.i % 10000000 == 0) {
        println(s"i = ${Counter.i} newIp = $newIpValue, newRegs = $newRegs")
      }
      Counter.++()
      execute(ipReg, newRegs, program)
    }
  }

  def executeUntilHalts(ipReg: Int)(initReg: List[Register])(program: Vector[ExecutionUnit]): List[Register] = {
    execute(ipReg, initReg, program)
  }

  def executeProgram(programLines: List[String]): Long = {
    val ipReg = parseIpReg(programLines)
    val initReg = regs(0, 0, 0, 0, 0, 0)
    val program = parseProgram(programLines)
    val registers = executeUntilHalts(ipReg)(initReg)(program.toVector)
    registers.head.value
  }

  def solvePart2(): Int = {
    val N = 10551264
    var sum = 0
    for (i <- 1 to N) {
      if (N % i == 0) {
        sum += i
      }
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day19-input")
    val solution1 = executeProgram(input)
    println(solution1)

    val solution2 = solvePart2()
    println(solution2)
  }

}
