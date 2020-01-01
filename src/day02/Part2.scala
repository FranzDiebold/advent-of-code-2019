/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 2: 1202 Program Alarm
 * Part 2
 */
package day02

import scala.io.Source

import day02.IntcodeProgram.runProgram

object Part2 {
  def getProgramOutput(program: Array[Int], noun: Int, verb: Int): Int = {
    program(1) = noun
    program(2) = verb

    runProgram(program)(0)
  }

  def getInputForDesiredProgramOutput(program: Array[Int], desiredOutput: Int, noun: Int = 0, verb: Int = 0): Tuple2[Int, Int] = {
    if (getProgramOutput(program.clone, noun, verb) == desiredOutput) {
      (noun, verb)
    } else {
      getInputForDesiredProgramOutput(program, desiredOutput, noun + verb / 99, (verb + 1) % 100)
    }
  }

  def main(args: Array[String]): Unit = {
    val program: Array[Int] = Source
      .fromFile("day02/input.txt")
      .getLines
      .next()
      .split(',')
      .map(_.toInt)

    val desiredOutput = 19690720

    val (noun, verb) = getInputForDesiredProgramOutput(program, desiredOutput)

    println(s"The input noun and verb that cause the program to produce the desired output $desiredOutput is noun=$noun and verb=$verb.")
    println(s"The value `100 * noun + verb` is ${100*noun + verb}.")
  }
}
