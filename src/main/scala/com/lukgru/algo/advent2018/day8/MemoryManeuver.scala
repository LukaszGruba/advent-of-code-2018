package com.lukgru.algo.advent2018.day8

import com.lukgru.algo.advent2018.utils.InputLoader

object MemoryManeuver {

  case class Node(children: List[Node], metadata: List[Int])

  def parseTree(data: List[Int]): (Node, List[Int]) = {
    val numberOfChildren = data.head
    val sizeOfMetadata = data(1)
    var newData = data.drop(2)
    val children =
      for (_ <- 0 until numberOfChildren) yield {
        val (child, restOfData) = parseTree(newData)
        newData = restOfData
        child
      }
    val metadata = newData.take(sizeOfMetadata)
    (Node(children.toList, metadata), newData.drop(sizeOfMetadata))
  }

  def traverseTree[A](f: Node => A, reducer: (A, A) => A)(node: Node): A = {
    val nodesResult = f(node)
    if (node.children.nonEmpty) {
      val childrenResult = node.children.map(traverseTree(f, reducer)).reduce(reducer)
      reducer(nodesResult, childrenResult)
    } else {
      nodesResult
    }
  }

  def calcNodeValue(node: Node): Int = {
    if (node.children.isEmpty) node.metadata.sum
    else {
      node.metadata.flatMap { index =>
        if (index <= node.children.length) {
          Some(calcNodeValue(node.children(index - 1)))
        } else None
      }.sum
    }
  }

  def solvePart1(input: String): Int = {
    val data = input.split(" ").map(_.toInt).toList
    val (tree, _) = parseTree(data)
    traverseTree(_.metadata.sum, (a: Int, b: Int) => a + b)(tree)
  }

  def solvePart2(input: String): Int = {
    val data = input.split(" ").map(_.toInt).toList
    val (tree, _) = parseTree(data)
    calcNodeValue(tree)
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day8-input").head
    val solution1 = solvePart1(input)
    println(solution1)

    val solution2 = solvePart2(input)
    println(solution2)
  }

}
