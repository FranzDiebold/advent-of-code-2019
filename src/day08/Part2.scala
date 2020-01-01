/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 8: Space Image Format
 * Part 2
 */
package day08

import day08.Image.{Image, LayeredImage, readLayeredImageFromFile, TransparentPixel, getImageRepresentation}

object Part2 {
  def combineLayerPixels(topLayerPixel: Int, bottomLayerPixel: Int): Int = {
    if (topLayerPixel == TransparentPixel) {
      bottomLayerPixel
    } else {
      topLayerPixel
    }
  }

  def main(args: Array[String]): Unit = {
    val imageSize = (25, 6)
    val layeredImage: LayeredImage = readLayeredImageFromFile("day08/input.txt", imageSize)

    val startVisualImage: Image = List.fill(imageSize._2)(List.fill(imageSize._1)(2).toIndexedSeq).toIndexedSeq
    val visualImage: Image = layeredImage.foldLeft(startVisualImage)(
      (currentVisualImage: Image, currentLayer: Image) => currentVisualImage.zip(currentLayer)
        .map{ case (currentVisualImageRow: IndexedSeq[Int], currentLayerRow: IndexedSeq[Int]) => currentVisualImageRow.zip(currentLayerRow)
          .map{ case (currentVisualPixel: Int, currentLayerPixel: Int) => combineLayerPixels(currentVisualPixel, currentLayerPixel) }
        }
    )

    println(s"The visual decoded image looks as follows:")
    println(getImageRepresentation(visualImage))
  }
}
