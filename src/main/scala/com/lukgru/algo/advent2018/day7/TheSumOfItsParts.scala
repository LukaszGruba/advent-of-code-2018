package com.lukgru.algo.advent2018.day7

import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object TheSumOfItsParts {

  case class Worker(nodeId: String = "", timeRemaining: Int = 0)

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

  def traverseGraph(graph: Set[Node]): String = {
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

  def solvePart1(lines: List[String]): String = {
    val rules = parseRules(lines)
    val graph = constructGraph(rules)
    traverseGraph(graph)
  }

  def nodeProcessingTime(base: Int)(nodeId: String): Int = base + ('a' to 'z').indexOf(nodeId(0).toLower)

  def solvePart2(lines: List[String], workerPoolSize: Int, processingBase: Int = 1): Int = {
    val rules = parseRules(lines)
    val graph = constructGraph(rules)
    val workers = List.fill(workerPoolSize)(Worker())

    def performOneSecondOfWork(workers: List[Worker]): List[Worker] = workers.map(w => w.copy(timeRemaining = w.timeRemaining - 1))

    val calcTime = nodeProcessingTime(processingBase) _

    @tailrec
    def sec(workers: List[Worker], graph: Set[Node], visitedIds: Set[String], totalTime: Int): Int = {
      println(s"workers = $workers")
      println(s"graph = $graph")
      println(s"visitedIds = $visitedIds")
      println(s"totalTime = $totalTime")
      println("\n")
      if (workers.forall(_.timeRemaining <= 0) && graph.isEmpty) totalTime + 1
      else {
        val freeWorkers = workers.filter(w => w.timeRemaining <= 0)
        val justFinishedNodeIds = freeWorkers.filter(_.nodeId != "").map(_.nodeId)
        val availableNodes = graph.filter(n => n.predecessors.forall(visitedIds.contains))
          .toList
          .sortBy(_.id)
        val newWorkers = freeWorkers.zip(availableNodes.toStream.map(n => n.id) ++ Stream.iterate("")(identity))
            .map { case (_, nodeId) => Worker(nodeId, if (nodeId == "") 0 else calcTime(nodeId))}
        val newlyTakenNodes = newWorkers.map(w => w.nodeId)

        sec(performOneSecondOfWork(workers.diff(freeWorkers) ++ newWorkers), graph.filterNot(n => newlyTakenNodes.contains(n.id)), visitedIds ++ justFinishedNodeIds, totalTime + 1)
      }
    }
    sec(workers, graph, Set.empty, 0)
  }

  def main(args: Array[String]): Unit = {
    val lines = InputLoader.loadLines("day7-input")
    val solution1 = solvePart1(lines)
    println(solution1)

    val solution2 = solvePart2(lines, 5, 60)
    println(solution2)
  }

}
