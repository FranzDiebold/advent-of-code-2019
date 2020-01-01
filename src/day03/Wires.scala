/**
 * Wires
 */
package day03

import scala.util.matching.Regex
import scala.math.{min, max}

class InvalidInstruction extends Exception

object Wires {
  type Vector2 = Tuple2[Int, Int]
  type PathSegment = Tuple2[Vector2, Vector2]

  @throws(classOf[InvalidInstruction])
  def instructionToPathVector(instruction: String): Vector2 = {
    val instructionPattern: Regex = "^([URDL])(\\d+)$".r
    instruction match {
      case instructionPattern(direction, lengthStr) => {
        val length = lengthStr.toInt
        direction match {
          case "U" => (0, length)
          case "R" => (length, 0)
          case "D" => (0, -length)
          case "L" => (-length, 0)
        }
      }
      case _ => throw new InvalidInstruction
    }
  }

  def addVectors(a: Vector2, b: Vector2): Vector2 = (a._1 + b._1, a._2 + b._2)

  def instructionsToPathSegments(instructions: Array[String]): Seq[PathSegment] = {
    instructions
      .map(instructionToPathVector)
      .scan((0, 0))(addVectors)
      .sliding(2)
      .map((vectors: Array[Vector2]) => (vectors(0), vectors(1)))
      .toSeq
  }

  def getIntersectionPointsOfPathSegments(ps1: PathSegment, ps2: PathSegment): Seq[Vector2] = {
    val ps1XMin = min(ps1._1._1, ps1._2._1)
    val ps1XMax = max(ps1._1._1, ps1._2._1)
    val ps1YMin = min(ps1._1._2, ps1._2._2)
    val ps1YMax = max(ps1._1._2, ps1._2._2)
    val ps2XMin = min(ps2._1._1, ps2._2._1)
    val ps2XMax = max(ps2._1._1, ps2._2._1)
    val ps2YMin = min(ps2._1._2, ps2._2._2)
    val ps2YMax = max(ps2._1._2, ps2._2._2)

    val ps1IsHorizontal = ps1YMin == ps1YMax
    val ps2IsHorizontal = ps2YMin == ps2YMax
    (ps1IsHorizontal, ps2IsHorizontal) match {
      case (true, false) => {
        val ps1Y = ps1YMin
        val ps2X = ps2XMin
        if (ps1XMin <= ps2X && ps2X <= ps1XMax && ps2YMin <= ps1Y && ps1Y <= ps2YMax) {
          Seq((ps2X, ps1Y))
        } else {
          Seq()
        }
      }
      case (false, true) => {
        val ps1X = ps1XMin
        val ps2Y = ps2YMin
        if (ps2XMin <= ps1X && ps1X <= ps2XMax && ps1YMin <= ps2Y && ps2Y <= ps1YMax) {
          Seq((ps1X, ps2Y))
        } else {
          Seq()
        }
      }
      case (true, true) => {
        if (ps1YMin == ps2YMin) {
          val xMaxMin = max(ps1XMin, ps2XMin)
          val xMinMax = min(ps2XMax, ps2XMax)
          xMaxMin.to(xMinMax).map((x: Int) => (x, ps1YMin)).toSeq
        } else {
          Seq()
        }
      }
      case (false, false) => {
        if (ps1XMin == ps2XMin) {
          val yMaxMin = max(ps1YMin, ps2YMin)
          val yMinMax = min(ps2YMax, ps2YMax)
          yMaxMin.to(yMinMax).map((y: Int) => (ps1XMin, y)).toSeq
        } else {
          Seq()
        }
      }
    }
  }

  def getIntersectionPoints(wirePathSegments: Seq[Seq[PathSegment]]): Seq[Vector2] = {
    wirePathSegments(0)
      .map((ps1: PathSegment) => {
        wirePathSegments(1)
          .map((ps2: PathSegment) => getIntersectionPointsOfPathSegments(ps1, ps2))
          .reduce(_ ++ _)
      })
      .reduce(_ ++ _)
      .tail
  }
}
