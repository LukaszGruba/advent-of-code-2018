package com.lukgru.algo.advent2018.day16

import com.lukgru.algo.advent2018.day16.ChronalClassification._
import org.scalatest.FunSuite

class ChronalClassificationTest extends FunSuite {

  test("test addr") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = addr(1, 2, 0, reg)

    //then
    assert(out == regs(10, 3, 7, 11))
  }

  test("test addi") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = addi(1, 20, 0, reg)

    //then
    assert(out == regs(23, 3, 7, 11))
  }

  test("test mulr") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = mulr(1, 2, 0, reg)

    //then
    assert(out == regs(21, 3, 7, 11))
  }

  test("test muli") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = muli(1, 20, 0, reg)

    //then
    assert(out == regs(60, 3, 7, 11))
  }

  test("test banr") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = banr(1, 2, 0, reg)

    //then
    assert(out == regs(3, 3, 7, 11))
  }

  test("test bani") {
    //given
    val reg = regs(10, 30, 7, 11)

    //when
    val out = bani(1, 17, 0, reg)

    //then
    assert(out == regs(16, 30, 7, 11))
  }

  test("test borr") {
    //given
    val reg = regs(1, 30, 70, 11)

    //when
    val out = borr(1, 2, 0, reg)

    //then
    assert(out == regs(94, 30, 70, 11))
  }

  test("test bori") {
    //given
    val reg = regs(1, 30, 7, 11)

    //when
    val out = bori(1, 70, 0, reg)

    //then
    assert(out == regs(94, 30, 7, 11))
  }

  test("test setr") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = setr(1, 2, 0, reg)

    //then
    assert(out == regs(3, 3, 7, 11))
  }

  test("test seti") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = seti(10, 2, 0, reg)

    //then
    assert(out == regs(10, 3, 7, 11))
  }

  test("test gtir") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = gtir(1, 2, 0, reg)

    //then
    assert(out == regs(0, 3, 7, 11))
  }

  test("test gtri") {
    //given
    val reg = regs(100, 3, 7, 11)

    //when
    val out = gtri(1, 2, 0, reg)

    //then
    assert(out == regs(1, 3, 7, 11))
  }

  test("test gtrr") {
    //given
    val reg = regs(1, 3, 7, 11)

    //when
    val out = gtrr(1, 2, 0, reg)

    //then
    assert(out == regs(0, 3, 7, 11))
  }

  test("test eqir") {
    //given
    val reg = regs(10, 3, 1, 11)

    //when
    val out = eqir(1, 2, 0, reg)

    //then
    assert(out == regs(1, 3, 1, 11))
  }

  test("test eqri") {
    //given
    val reg = regs(13, 3, 7, 11)

    //when
    val out = eqri(1, 3, 0, reg)

    //then
    assert(out == regs(1, 3, 7, 11))
  }

  test("test eqrr") {
    //given
    val reg = regs(9, 7, 7, 11)

    //when
    val out = eqrr(1, 2, 0, reg)

    //then
    assert(out == regs(1, 7, 7, 11))
  }

  test("should return matching instructions") {
    //given
    val before = regs(3, 2, 1, 1)
    val after = regs(3, 2, 2, 1)

    //when
    val matching = getMatchingInstructions(allInstructions)(2, 1, 2)(before, after)

    //then
    assert(matching.map(_.opcode) == Set("mulr", "addi", "seti"))
  }

}
