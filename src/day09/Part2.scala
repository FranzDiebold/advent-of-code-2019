/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 9: Sensor Boost
 * Part 2
 */
package day09

import day09.IntcodeProgram.{Program, readProgramFromFile, ProgramOutput, runProgram}

object Part2 {
  def main(args: Array[String]): Unit = {
    val program: Program = readProgramFromFile("day09/input.txt")

    val input = List(BigInt(2))
    val programOutput: ProgramOutput = runProgram(program, input.iterator)
    println(s"The coordinates of the distress signal are ${programOutput(0)}.")
  }
}
