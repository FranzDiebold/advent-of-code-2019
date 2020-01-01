/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 2: 1202 Program Alarm
 * Part 1
 */
package day02

import scala.io.Source

import day02.IntcodeProgram.runProgram

object Part1 {
  def main(args: Array[String]): Unit = {
    val program: Array[Int] = Source
      .fromFile("day02/input.txt")
      .getLines
      .next()
      .split(',')
      .map(_.toInt)
    program(1) = 12
    program(2) = 2

    val outputProgram = runProgram(program)

    println(s"The value at position 0 after the program halts is ${outputProgram(0)}.")
  }
}
