/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 7: Amplification Circuit
 * Part 1
 */
package day07

import scala.io.Source

import day05.IntcodeProgram.{Program, ProgramOutput, runProgram}

object Part1 {
  def runAmplifier(program: Program, phaseSetting: Int, inputSignal: Int = 0): Int = {
    val input = List(phaseSetting, inputSignal)
    val programOutput: ProgramOutput = runProgram(program, input.iterator)
    val outputSignal: Int = programOutput.last
    outputSignal
  }

  def main(args: Array[String]): Unit = {
    val program: Program = Source
      .fromFile("day07/input.txt")
      .getLines
      .next()
      .split(',')
      .map(_.toInt)

    val numberOfAmplifiers = 5

    val highestSignal: Int = 0.to(numberOfAmplifiers - 1)
      .permutations
      .map((phaseSettings: IndexedSeq[Int]) => {
        phaseSettings.foldLeft(0)(
          (inputOutputSignal: Int, phaseSetting: Int) => runAmplifier(program.clone(), phaseSetting, inputOutputSignal)
        )
      })
      .max
    val out: Int = runAmplifier(program, 0)
    println(s"The highest signal that can be sent to the thrusters is ${highestSignal}.")
  }
}
