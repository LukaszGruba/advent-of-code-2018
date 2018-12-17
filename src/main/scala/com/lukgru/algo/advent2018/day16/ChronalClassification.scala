package com.lukgru.algo.advent2018.day16

import com.lukgru.algo.advent2018.utils.InputLoader

object ChronalClassification {

  case class Scenario(insNo: Int, args: List[Int], before: List[Register], after: List[Register])

  case class Register(id: Int, value: Int)

  case class Instruction(opcode: String, op: (Int, Int, Int, List[Register]) => List[Register])

  def addr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value + reg(b).value))

  def addi(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value + b))

  def mulr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value * reg(b).value))

  def muli(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value * b))

  def banr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value & reg(b).value))

  def bani(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value & b))

  def borr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value | reg(b).value))

  def bori(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value | b))

  def setr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = reg(a).value))

  def seti(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = a))

  def gtir(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = if (a > reg(b).value) 1 else 0))

  def gtri(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = if (reg(a).value > b) 1 else 0))

  def gtrr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = if (reg(a).value > reg(b).value) 1 else 0))

  def eqir(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = if (a == reg(b).value) 1 else 0))

  def eqri(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = if (reg(a).value == b) 1 else 0))

  def eqrr(a: Int, b: Int, c: Int, reg: List[Register]): List[Register] =
    reg.updated(c, reg(c).copy(value = if (reg(a).value == reg(b).value) 1 else 0))

  val allInstructions = Set(
    Instruction("addr", addr),
    Instruction("addi", addi),
    Instruction("mulr", mulr),
    Instruction("muli", muli),
    Instruction("banr", banr),
    Instruction("bani", bani),
    Instruction("borr", borr),
    Instruction("bori", bori),
    Instruction("setr", setr),
    Instruction("seti", seti),
    Instruction("gtir", gtir),
    Instruction("gtri", gtri),
    Instruction("gtrr", gtrr),
    Instruction("eqir", eqir),
    Instruction("eqri", eqri),
    Instruction("eqrr", eqrr)
  )

  def regs(a: Int, b: Int, c: Int, d: Int): List[Register] = {
    List(Register(0, a), Register(1, b), Register(2, c), Register(3, d))
  }

  def getMatchingInstructions(instructions: Set[Instruction])
                             (a: Int, b: Int, c: Int)
                             (before: List[Register], after: List[Register]): Set[Instruction] = {
    instructions.filter { instruction =>
      instruction.op(a, b, c, before) == after
    }
  }

  def parseScenario(scenario: List[String]): Scenario = ???

  def countNOrMoreBehaviours(instructions: Set[Instruction])(n: Int)(lines: List[String]): Int = ???

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day16-input1")
    val numberOfTrippleMatchingDatasets = countNOrMoreBehaviours(allInstructions)(3)(input)
    println(numberOfTrippleMatchingDatasets)
  }

}
