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

/**
  * The top level object of the Snake application.
  */
object Main {

  import CubeState._

  /**
    * Main entry point of the application.
    *
    * @param args the array of options and parameters passed on 
    * the command line.
    */
  def main(args: Array[String]) {
    val sols = solveSnake(snake)
    println(sols)
  }
}
