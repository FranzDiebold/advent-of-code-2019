/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 4: Secure Container
 * Part 1
 */
package day04

object Part1 {
  def passwordMeetsCriteria(password: Int): Boolean = {
    val passwordDigitDifferences: Seq[Int] = password
      .toString
      .map(_.asDigit)
      .sliding(2)
      .map((digits: IndexedSeq[Int]) => digits(1) - digits(0))
      .toSeq

    val isSixDigitNumber = password >= 1e5 && password < 1e6
    val twoAdjacentDigits = passwordDigitDifferences.contains(0)
    val digitsNeverDecrease = passwordDigitDifferences.min >= 0
    return isSixDigitNumber && twoAdjacentDigits && digitsNeverDecrease
  }

  def main(args: Array[String]): Unit = {
    val rangeMin = 372037
    val rangeMax = 905157

    val numberOfPasswords = rangeMin.to(rangeMax)
      .map(passwordMeetsCriteria)
      .map{
        case true => 1
        case _ => 0
      }
      .sum

    println(s"The number of different passwords within the range (${rangeMin} - ${rangeMax}) that meet the criteria is ${numberOfPasswords}.")
  }
}
