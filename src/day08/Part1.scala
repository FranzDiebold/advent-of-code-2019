/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 8: Space Image Format
 * Part 1
 */
package day08

import day08.Image.{Image, LayeredImage, readLayeredImageFromFile}

object Part1 {
  def main(args: Array[String]): Unit = {
    val layeredImage: LayeredImage = readLayeredImageFromFile("day08/input.txt", (25, 6))

    val fewestZeroDigitsLayerCounts: Map[Int, Int] = layeredImage
      .map((image: Image) => image.reduce(_ ++ _).groupBy(identity).view.mapValues(_.size).toMap)
      .minBy((layerCounts: Map[Int, Int]) => layerCounts(0))

    val productResult: Int = fewestZeroDigitsLayerCounts(1) * fewestZeroDigitsLayerCounts(2)
    println(s"The number of 1 digits multiplied by the number of 2 digits in the layer that contains the fewest 0 digits is ${productResult}.")
  }
}
