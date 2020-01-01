/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 4: Secure Container
 * Part 2
 */
package day04

object Part2 {
  def passwordMeetsAllCriteria(password: Int): Boolean = {
    val passwordDigitDifferences: Seq[Int] = password
      .toString
      .map(_.asDigit)
      .sliding(2)
      .map((digits: IndexedSeq[Int]) => digits(1) - digits(0))
      .toSeq

    val isSixDigitNumber = password >= 1e5 && password < 1e6
    val twoAdjacentDigits = passwordDigitDifferences.contains(0)
    val digitsNeverDecrease = passwordDigitDifferences.min >= 0
    val atMostTwoAdjacentDigits = (1 +: passwordDigitDifferences :+ 1)
      .sliding(3)
      .map((differences: Seq[Int]) => differences(0) != 0 && differences(1) == 0 && differences(2) != 0)
      .contains(true)
    return isSixDigitNumber && twoAdjacentDigits && digitsNeverDecrease && atMostTwoAdjacentDigits
  }

  def main(args: Array[String]): Unit = {
    val rangeMin = 372037
    val rangeMax = 905157

    val numberOfPasswords = rangeMin.to(rangeMax)
      .map(passwordMeetsAllCriteria)
      .map{
        case true => 1
        case _ => 0
      }
      .sum

    println(s"The number of different passwords within the range (${rangeMin} - ${rangeMax}) that meet all the criteria is ${numberOfPasswords}.")
  }
}
