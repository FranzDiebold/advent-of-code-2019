/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 5: Sunny with a Chance of Asteroids
 * Part 2
 */
package day05

import scala.io.Source

import day05.IntcodeProgram.{Program, ProgramOutput, runProgram}

object Part2 {
  def main(args: Array[String]): Unit = {
    val program: Program = Source
      .fromFile("day05/input.txt")
      .getLines
      .next()
      .split(',')
      .map(_.toInt)

    val input = List(5)

    val programOutput: ProgramOutput = runProgram(program, input.iterator)
    val diagnosticCode: Int = programOutput.last

    println(s"The diagnostic code is ${diagnosticCode}.")
  }
}
