/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 1: The Tyranny of the Rocket Equation
 * Part 2
 */

import scala.io.Source

object Part2 {
  def massToFuel(mass: Int): Int = mass / 3 - 2

  def fuelToFuel(totalFuel: Int, newFuel: Int): Tuple2[Int, Int] = {
    if (newFuel <= 0) {
      (totalFuel, 0)
    } else {
      fuelToFuel(totalFuel + newFuel, massToFuel(newFuel))
    }
  }

  def massToTotalFuel(mass: Int): Int = {
    val baseFuel = massToFuel(mass)
    val (totalFuel, _) = fuelToFuel(0, baseFuel)
    totalFuel
  }

  def main(args: Array[String]): Unit = {
    val totalFuel: Int = Source
      .fromFile("day01/input.txt")
      .getLines
      .map(_.toInt)
      .map(massToTotalFuel)
      .sum

    println(s"The sum of the total fuel requirements for all of the modules is $totalFuel.")
  }
}
