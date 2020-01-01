/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 5: Sunny with a Chance of Asteroids
 * Part 1
 */
package day05

import scala.io.Source

import day05.IntcodeProgram.{Program, ProgramOutput, runProgram}

object Part1 {
  def main(args: Array[String]): Unit = {
    val program: Program = Source
      .fromFile("day05/input.txt")
      .getLines
      .next()
      .split(',')
      .map(_.toInt)

    val input = List(1)

    val programOutput: ProgramOutput = runProgram(program, input.iterator)
    val diagnosticTestsOutputs: ProgramOutput = programOutput.dropRight(1)
    val diagnosticTestsSuccessful: Boolean = diagnosticTestsOutputs.map(_ == 0).reduce(_ && _)
    val diagnosticCode: Int = programOutput.last

    if (diagnosticTestsSuccessful) {
      println(s"All diagnostic tests were successful.")
    } else {
      println(s"Not all diagnostic tests were successful. The diagnostic tests output is ${diagnosticTestsOutputs}.")
    }
    println(s"The diagnostic code is ${diagnosticCode}.")
    println(s"The output of the program is ${programOutput}.")
  }
}
