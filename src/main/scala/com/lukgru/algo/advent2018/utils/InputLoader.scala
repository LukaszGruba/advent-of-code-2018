package com.lukgru.algo.advent2018.utils

import scala.io.Source

object InputLoader {

  def loadLines(resourcePath: String): List[String] = Source.fromResource(resourcePath).mkString.lines.toList

}
