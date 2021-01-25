/*
 * This file is part of COMP3000 Assignment 1.
 *
 * Copyright (C) 2020 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.snake

object CubeState {

  /**
    * snake segments
    */
  val snake = List(3, 1, 1, 2, 1, 2, 1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2)

  /**
    * Case class for the directions
    * note: getPossibleDirections must always return directions in the order below
    */
  sealed abstract class Direction
  case object XPos extends Direction
  case object XNeg extends Direction
  case object YPos extends Direction
  case object YNeg extends Direction
  case object ZPos extends Direction
  case object ZNeg extends Direction

  /**
    * Case class for the puzzle/cube state
    * Start state is before anything has been put in the cube; it contains:
    *  - the segments of the snake
    *  - the start position in the cube of the snake
    * OtherState is the puzzle state after the first move; it contains:
    *  - the segments of the snake
    *  - what parts of the cube are occupied
    *  - the end position of the last move to date
    *  - the direction of the last move to date
    *  - the number of moves to date
    */
  sealed abstract class CubeState
  case class StartState(segments: List[Int], start: (Int,Int,Int)) extends CubeState
  case class OtherState(segments: List[Int], occupied:Set[(Int,Int,Int)],
                        end: (Int,Int,Int), dir: Direction, numMoves: Int) extends CubeState

  /**
    * Solve the snake cube puzzle with the supplied snake segments
    *
    * @param segments the snake segments
    * @return a list of solutions where each solution looks like:
    *  ((x, y, z), List(direction1, direction2, ... , directionk))
    * where (x, y, z) is the start position in the cube of the snake and directioni are the
    * sequence of directions for each snake segment
    */
  // hint: for, map, match
  def solveSnake(segments: List[Int]): List[((Int,Int,Int), List[Direction])] =
  {
    // FIXME
    List()
  }

  def prependDir(dir: Direction, solutions: List[List[Direction]]): List[List[Direction]] =
    //for every dirList (List[Direction]) prepend dir to dirList
    solutions.map {
      case dirList => dir :: dirList
    }

  def prependStart(xyz: (Int,Int,Int), solutions: List[List[Direction]]):
                                          List[((Int,Int,Int), List[Direction])] =
    // for every dirList (List[Direction]) create a 2Tuple with (xyz, dirList)
    solutions.map {
      case dirList => (xyz, dirList)
    }

   def removeOption(a : Option[CubeState]): CubeState =
    a match {
        case None => null
        case Some(c) => c
      }

  // hint: match, filter
  def getSolutions(state: CubeState): Option[List[List[Direction]]] =
    state match {
        case StartState(segments, start) => return Some(List(List()))
        case OtherState(segments, occupied, end, dir, numMoves) => return Some(List(List()))
    }
    //None

  // Move dir in the given Direction
  def getIncrement(dir: Direction):(Int,Int,Int) =
  // match case to all directions and increment/decrement
    dir match {
      case XPos => (+1, 0, 0)
      case XNeg => (-1, 0, 0)
      case YPos => (0, +1, 0)
      case YNeg => (0, -1, 0)
      case ZPos => (0, 0, +1)
      case ZNeg => (0, 0, -1)
      case _  => (0, 0, 0)
    }

  // based only on the state's direction and numMoves
  // hint: match
  def getPossibleDirections(state: CubeState): List[Direction] = 
    state match {
      case StartState(segments, start) => List(XPos) // First move is always XPos
      case OtherState(segments, occupied, end, dir, numMoves) => numMoves match {
        case 0 => List(XPos) // Second move is always YPos
        case 1 => List(YPos) // Second move is always YPos
        case _ => dir match {
          case XPos => List(YPos, YNeg, ZPos, ZNeg)
          case XNeg => List(YPos, YNeg, ZPos, ZNeg)
          case YPos => List(XPos, XNeg, ZPos, ZNeg)
          case YNeg => List(XPos, XNeg, ZPos, ZNeg)
          case ZPos => List(XPos, XNeg, YPos, YNeg)
          case ZNeg => List(XPos, XNeg, YPos, YNeg)
        } 
      }
      case _ => List()
    }

  def combineXYZ (dir1: (Int, Int, Int), dir2: (Int, Int, Int), m: Int): (Int, Int, Int) =
    (dir1._1+m.*(dir2._1),dir1._2+m.*(dir2._2),dir1._3+m.*(dir2._3))

  // hint: match
  def tryMove(state: CubeState, move: Direction): Option[CubeState] = 
  state match {
    case StartState(segments, start) => Some(OtherState(snake, Set(start) + 
            (combineXYZ(start, (1, 0, 0), 1)) + (combineXYZ(start,(1, 0, 0), 2)), combineXYZ(start,(1, 0, 0), 2), XPos, 1))
    case OtherState(segments, occupied, end, dir, numMoves) => 

    val move1 = combineXYZ(end, getIncrement(move), 1)
    val move2 = combineXYZ(end, getIncrement(move), 2)

    move match {
      case XPos => if (move1._1 >= 3) None
                   else if (segments(numMoves) == 2) Some(OtherState(segments, occupied + (move1) + (move2), move1, XPos, numMoves + 1))
                   else Some(OtherState(segments, occupied + (move1), move1, XPos, numMoves + 1))
      case XNeg => if (move1._1 < 0) None
                   else if (segments(numMoves) == 2) Some(OtherState(segments, occupied + (move1) + (move2), move1, XNeg, numMoves + 1))
                   else Some(OtherState(segments, occupied + (move1), move1, XNeg, numMoves + 1))
      case YPos => if (move1._1 >= 3) None
                   else if (segments(numMoves) == 2) Some(OtherState(segments, occupied + (move1) + (move2), move1, YPos, numMoves + 1))
                   else Some(OtherState(segments, occupied + (move1), move1, YPos, numMoves + 1))
      case YNeg => if (move1._1 < 0) None
                   else if (segments(numMoves) == 2) Some(OtherState(segments, occupied + (move1) + (move2), move1, YNeg, numMoves + 1))
                   else Some(OtherState(segments, occupied + (move1), move1, YNeg, numMoves + 1))
      case ZPos => if (move1._1 >= 3) None
                   else if (segments(numMoves) == 2) Some(OtherState(segments, occupied + (move1) + (move2), move1, ZPos, numMoves + 1))
                   else Some(OtherState(segments, occupied + (move1), move1, ZPos, numMoves + 1))
      case ZNeg => if (move1._1 < 0) None
                   else if (segments(numMoves) == 2) Some(OtherState(segments, occupied + (move1) + (move2), move1, ZNeg, numMoves + 1))
                   else Some(OtherState(segments, occupied + (move1), move1, ZNeg, numMoves + 1))
      case _ => Some(OtherState(snake, Set(),(0, 0, 0), ZNeg, -1))
    }
  }

  def show(state: CubeState)
  {
    state match
    {
    case StartState(_, start) => println("start " + start)
    case OtherState(_, s, end, dir, numMoves) =>
            def d(x: Int, y: Int, z: Int): String = if(s.contains((x, y, z))) "1" else "0"
            println(d(0, 2, 0)+d(1, 2, 0)+d(2, 2, 0)+" "
                          + d(0, 2, 1)+d(1, 2, 1)+d(2, 2, 1)+" "
                          + d(0, 2, 2)+d(1, 2, 2)+d(2, 2, 2)+" "+end)
            println(d(0, 1, 0)+d(1, 1, 0)+d(2, 1, 0)+" "
                          + d(0, 1, 1)+d(1, 1, 1)+d(2, 1, 1)+" "
                          + d(0, 1, 2)+d(1, 1, 2)+d(2, 1, 2)+" "+dir)
            println(d(0, 0, 0)+d(1, 0, 0)+d(2, 0, 0)+" "
                          + d(0, 0, 1)+d(1, 0, 1)+d(2, 0, 1)+" "
                          + d(0, 0, 2)+d(1, 0, 2)+d(2, 0, 2)+" "+numMoves)
    }
  }

  def show(state: Option[CubeState])
  {
    state match
    {
    case Some(st) => show(st)
    case None     => println("None")
    }
  }

  def toSet(a: List[Int]): Set[(Int,Int,Int)] =
  {
    if(a.size != 27) throw new Exception("toSet list must have 27 elements (not " + a.size + ")")
    var b = Set[(Int,Int,Int)]()
    // d(x, y, z, i) adds (x, y, z) to the set if a(i) is 1
    def d(x: Int, y: Int, z: Int, i: Int) { if(a(i) == 1) b = b + ((x, y, z)) }
    d(0, 2, 0, 0)
    d(1, 2, 0, 1)
    d(2, 2, 0, 2)
    d(0, 2, 1, 3)
    d(1, 2, 1, 4)
    d(2, 2, 1, 5)
    d(0, 2, 2, 6)
    d(1, 2, 2, 7)
    d(2, 2, 2, 8)
    d(0, 1, 0, 9)
    d(1, 1, 0, 10)
    d(2, 1, 0, 11)
    d(0, 1, 1, 12)
    d(1, 1, 1, 13)
    d(2, 1, 1, 14)
    d(0, 1, 2, 15)
    d(1, 1, 2, 16)
    d(2, 1, 2, 17)
    d(0, 0, 0, 18)
    d(1, 0, 0, 19)
    d(2, 0, 0, 20)
    d(0, 0, 1, 21)
    d(1, 0, 1, 22)
    d(2, 0, 1, 23)
    d(0, 0, 2, 24)
    d(1, 0, 2, 25)
    d(2, 0, 2, 26)
    b
  }
}
