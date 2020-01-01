/**
 * Image utility functions and types.
 */
package day08

import scala.io.Source

object Image {
  type Image = IndexedSeq[IndexedSeq[Int]]
  type LayeredImage = IndexedSeq[Image]

  val BlackPixel = 0
  val WhitePixel = 1
  val TransparentPixel = 2

  def readRawLayeredImageFromFile(fileName: String): Seq[Int] = Source
    .fromFile(fileName)
    .getLines
    .next()
    .map(_.asDigit)

  def rawLayeredImageToLayeredImage(rawLayeredImage: Seq[Int], imageSize: Tuple2[Int, Int]): LayeredImage = {
    val valuesPerLayer = imageSize._1 * imageSize._2
    rawLayeredImage
      .sliding(valuesPerLayer, valuesPerLayer)
      .map(
        (rawLayer: Seq[Int]) => rawLayer
          .sliding(imageSize._1, imageSize._1)
          .map(_.toIndexedSeq)
          .toIndexedSeq
      )
      .toIndexedSeq
  }

  def readLayeredImageFromFile(fileName: String, imageSize: Tuple2[Int, Int]): LayeredImage = rawLayeredImageToLayeredImage(readRawLayeredImageFromFile(fileName), imageSize)

  def pixelRepresentation(pixel: Int): String = {
    pixel match {
      case BlackPixel => " "
      case WhitePixel => "x"
      case TransparentPixel => "#"
      case _ => "?"
    }
  }

  def getImageRepresentation(image: Image): String = {
    image
      .map((row: IndexedSeq[Int]) => row
        .map((pixel: Int) => pixelRepresentation(pixel))
        .reduce(_ + _) + "\n"
      )
      .reduce(_ + _)
  }
}
