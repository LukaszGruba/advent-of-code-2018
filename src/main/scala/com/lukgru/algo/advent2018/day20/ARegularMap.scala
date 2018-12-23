package com.lukgru.algo.advent2018.day20

import com.lukgru.algo.advent2018.day20.ARegularMap.DoorOrientation.DoorOrientation
import com.lukgru.algo.advent2018.utils.InputLoader

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

  def findAdjacentRooms(room: Room, regMap: RegMap): Set[Room] = {
    def roomBehindTheDoor(door: Door): Room = door match {
      case d if d.pos.y < room.pos.y => Room(Position(room.pos.x, room.pos.y - 2))
      case d if d.pos.y > room.pos.y => Room(Position(room.pos.x, room.pos.y + 2))
      case d if d.pos.x < room.pos.x => Room(Position(room.pos.x - 2, room.pos.y))
      case d if d.pos.x > room.pos.x => Room(Position(room.pos.x + 2, room.pos.y))
    }

    val horizontalDoors = regMap.doors
      .filter(_.orientation == DoorOrientation.Horizontal)
      .filter { d =>
        d.pos.x == room.pos.x && Math.abs(d.pos.y - room.pos.y) == 1
      }
    val verticalDoors = regMap.doors
      .filter(_.orientation == DoorOrientation.Vertical)
      .filter { d =>
        Math.abs(d.pos.x - room.pos.x) == 1 && d.pos.y == room.pos.y
      }
    (horizontalDoors ++ verticalDoors).map(roomBehindTheDoor)
  }

  def constructRoomsMap(regMap: RegMap): Map[Room, List[Room]] = {

    @tailrec
    def constructMapRec(toVisit: Queue[(Room, List[Room])], map: Map[Room, List[Room]]): Map[Room, List[Room]] =
      if (toVisit.isEmpty) map
      else {
        val ((room, pathToRoom), remainingRooms) = toVisit.dequeue
        val newMap =
          if (!map.contains(room)) {
            map + (room -> pathToRoom)
          } else {
            map
          }
        val notVisitedAdjacentRooms = findAdjacentRooms(room, regMap)
          .filterNot(r => map.contains(r))
          .map(r => (r, pathToRoom :+ room))
        val updatedToVisit = remainingRooms ++ notVisitedAdjacentRooms
        constructMapRec(updatedToVisit, newMap)
      }

    constructMapRec(Queue((Room(Position(0, 0)), Nil)), Map.empty)
  }

  def findShortestPathLengthToMostDistantRoom(regex: String): Int = {
    val regMap = parseRegMapPositions(regex)
    val roomsWithPaths = constructRoomsMap(regMap)
    roomsWithPaths.values
      .map(path => path.length)
      .max
  }

  def main(args: Array[String]): Unit = {
    val input = InputLoader.loadLines("day20-input").head
    val solution1 = findShortestPathLengthToMostDistantRoom(input)
    println(solution1)
  }

}
