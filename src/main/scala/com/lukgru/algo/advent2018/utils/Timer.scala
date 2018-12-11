package com.lukgru.algo.advent2018.utils

object Timer {

  def logTime[R](task: => R): R = {
    val start = System.currentTimeMillis()
    val result = task
    val stop = System.currentTimeMillis()
    println(s"Execution time: ${stop - start}\n")
    result
  }

}
