package com.lukgru.algo.advent2018.utils

import scala.io.Source

object InputLoader {

  def load(resourcePath: String): String = Source.fromResource(resourcePath).mkString

}
