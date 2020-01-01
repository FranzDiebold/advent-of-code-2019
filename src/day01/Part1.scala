/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 1: The Tyranny of the Rocket Equation
 * Part 1
 */

import scala.io.Source

object Part1 {
  def massToFuel(mass: Int): Int = mass / 3 - 2

  def main(args: Array[String]): Unit = {
    val totalFuel: Int = Source
      .fromFile("input.txt")
      .getLines
      .map(_.toInt)
      .map(massToFuel)
      .sum

    println(s"The sum of the fuel requirements for all of the modules is $totalFuel.")
  }
}
