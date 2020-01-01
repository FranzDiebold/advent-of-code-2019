/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 7: Amplification Circuit
 * Part 2
 */
package day07

import scala.io.Source

import day05.IntcodeProgram.{Program, ProgramOutput, runProgram}

object Part2 {
  def runAmplifier(program: Program, phaseSetting: Int, inputSignal: Int = 0): Int = {
    val input = List(phaseSetting, inputSignal)
    println(input)
    val programOutput: ProgramOutput = runProgram(program, input.iterator)
    println(programOutput)
    val outputSignal: Int = programOutput.last
    outputSignal
  }

  def main(args: Array[String]): Unit = {
    /*
    val program: Program = Source
      .fromFile("day07/input.txt")
      .getLines
      .next()
      .split(',')
      .map(_.toInt)
      */
    val program: Program = Array(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
    println(runProgram(program, Array(9, 1, 2, 3, 4, 5).iterator))

    /*
    val numberOfAmplifiers = 5

    val startPhaseSetting = 5
    val highestSignal: Int = startPhaseSetting.to(startPhaseSetting + numberOfAmplifiers - 1)
      .permutations
      .map((phaseSettings: IndexedSeq[Int]) => {
        phaseSettings.foldLeft(0)(
          (inputOutputSignal: Int, phaseSetting: Int) => runAmplifier(program.clone(), phaseSetting, inputOutputSignal)
        )
      })
      .max
    val out: Int = runAmplifier(program, 0)
    println(s"The highest signal that can be sent to the thrusters is ${highestSignal}.")
    */
  }
}
