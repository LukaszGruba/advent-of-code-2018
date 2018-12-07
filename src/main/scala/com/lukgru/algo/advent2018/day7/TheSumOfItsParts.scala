package com.lukgru.algo.advent2018.day7

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object TheSumOfItsParts {

  case class Rule(predecessorsName: String, successorsName: String)

  case class Node(id: String, predecessors: Set[String], successors: Set[String], processed: Boolean = false)

  def parseRules(lines: List[String]): List[Rule] = {
    val pattern = "Step (.*) must be finished before step (.*) can begin.".r
    lines.map(line => {
      val pattern(predecessor, successor) = line
      Rule(predecessor, successor)
    })
  }

  def constructGraph(rules: List[Rule]): Set[Node] = {
    val successorsMap = rules.groupBy(_.predecessorsName)
      .map { case (pre, successors) => (pre, successors.map(_.successorsName)) }
    val predecessorsMap = rules.groupBy(_.successorsName)
      .map { case (succ, predecessors) => (succ, predecessors.map(_.predecessorsName)) }
    val nodeIds = rules.flatMap(r => List(r.successorsName, r.predecessorsName)).distinct
    nodeIds.map { nodeId =>
      Node(nodeId, predecessorsMap.getOrElse(nodeId, List.empty).toSet, successorsMap.getOrElse(nodeId, List.empty).toSet)
    }.toSet
  }

  def traverseGraph(lines: List[String]): String = {
    val rules = parseRules(lines)
    val graph = constructGraph(rules)

    @tailrec
    def traverse(graph: Set[Node], visitedIds: Set[String], path: String): String = {
      if (graph.size == 1) path + graph.head.id
      else {
        val node = graph.filter(n => {
          n.predecessors.forall(visitedIds.contains)
        }).minBy(_.id)
        traverse(graph - node, visitedIds + node.id, path + node.id)
      }
    }
    traverse(graph, Set.empty, "")
  }

  def main(args: Array[String]): Unit = {
    val lines = InputLoader.loadLines("day7-input")
    val solution1 = traverseGraph(lines)
    println(solution1)
  }

}
