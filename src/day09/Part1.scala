/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 9: Sensor Boost
 * Part 1
 */
package day09

import day09.IntcodeProgram.{Program, readProgramFromFile, ProgramOutput, runProgram}

object Part1 {
  def main(args: Array[String]): Unit = {
    val program: Program = readProgramFromFile("day09/input.txt")

    val input = List(BigInt(1))
    val programOutput: ProgramOutput = runProgram(program, input.iterator)
    println(s"The BOOST keycode is ${programOutput(0)}.")
  }
}
