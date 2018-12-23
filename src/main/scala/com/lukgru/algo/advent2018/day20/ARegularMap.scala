package com.lukgru.algo.advent2018.day20

import com.lukgru.algo.advent2018.day20.ARegularMap.DoorOrientation.DoorOrientation
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec

object ARegularMap {

  object DoorOrientation extends Enumeration {
    type DoorOrientation = Value
    val Horizontal, Vertical = Value
  }

  case class Position(x: Int, y: Int)

  case class Room(pos: Position)

  case class Door(pos: Position, orientation: DoorOrientation)

  case class RegMap(rooms: Set[Room], doors: Set[Door])

  def p = Position

  def nextRoomDoorPair(room: Room, dir: Char): (Door, Room) = dir match {
    case 'N' => (Door(room.pos.copy(y = room.pos.y + 1), DoorOrientation.Horizontal), Room(room.pos.copy(y = room.pos.y + 2)))
    case 'W' => (Door(room.pos.copy(x = room.pos.x - 1), DoorOrientation.Vertical), Room(room.pos.copy(x = room.pos.x - 2)))
    case 'S' => (Door(room.pos.copy(y = room.pos.y - 1), DoorOrientation.Horizontal), Room(room.pos.copy(y = room.pos.y - 2)))
    case 'E' => (Door(room.pos.copy(x = room.pos.x + 1), DoorOrientation.Vertical), Room(room.pos.copy(x = room.pos.x + 2)))
  }

  def isDir(c: Char): Boolean = "NWES".contains(c)

  @tailrec
  def recParse(positionStack: List[Position], lastRoom: Room, remainingPath: List[Char], currentRegMap: RegMap): RegMap =
    remainingPath match {
      case Nil => currentRegMap
      case '(' +: dirs =>
        recParse(lastRoom.pos +: positionStack, lastRoom, dirs, currentRegMap)
      case ')' +: dirs =>
        recParse(positionStack.tail, Room(positionStack.head), dirs, currentRegMap)
      case '|' +: dirs =>
        recParse(positionStack, Room(positionStack.head), dirs, currentRegMap)
      case (dir: Char) +: dirs if isDir(dir) =>
        val (newDoor, newRoom) = nextRoomDoorPair(lastRoom, dir)
        val newDoors = currentRegMap.doors + newDoor
        val newRooms = currentRegMap.rooms + newRoom
        val newRegMap = RegMap(newRooms, newDoors)
        recParse(positionStack, newRoom, dirs, newRegMap)
    }

  def parseRegMapPositions(mapRegex: String): RegMap = {
    val raw = mapRegex.filter(c => c != '^' && c != '$')
    recParse(List.empty, Room(Position(0, 0)), raw.toList, RegMap(Set.empty, Set.empty))
  }

  def solvePart1(regex: String): Int = {
    val regMap = parseRegMapPositions(regex)
    ???
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day20-input").head
    val solution1 = solvePart1(input)
    println(solution1)
  }

}
