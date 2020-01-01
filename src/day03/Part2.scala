/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 3: Crossed Wires
 * Part 2
 */
package day03

import scala.math.{abs, min}
import scala.io.Source

import day03.Wires.{Vector2, PathSegment, instructionsToPathSegments, getIntersectionPoints, getIntersectionPointsOfPathSegments}

class WireDoesNotReachPointException extends Exception

object Part2 {
  @throws(classOf[WireDoesNotReachPointException])
  def stepsToReachPoint(targetPoint: Vector2, wirePathSegmentIter: Iterator[PathSegment], currentSteps: Int = 0): Int = {
    if (!wirePathSegmentIter.hasNext) {
      throw new WireDoesNotReachPointException
    } else {
      val currentPathSegment: PathSegment = wirePathSegmentIter.next()
      val intersectionPoints: Seq[Vector2] = getIntersectionPointsOfPathSegments(currentPathSegment, (targetPoint, targetPoint))
      if (!intersectionPoints.isEmpty) {
        val newSteps: Int = abs(currentPathSegment._1._1 - targetPoint._1) + abs(currentPathSegment._1._2 - targetPoint._2)
        currentSteps + newSteps
      } else {
        val newSteps: Int = abs(currentPathSegment._1._1 - currentPathSegment._2._1) + abs(currentPathSegment._1._2 - currentPathSegment._2._2)
        stepsToReachPoint(targetPoint, wirePathSegmentIter, currentSteps + newSteps)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val wirePathSegments: Seq[Seq[PathSegment]] = Source
      .fromFile("day03/input.txt")
      .getLines
      .map(_.split(','))
      .map(instructionsToPathSegments)
      .toSeq

    val intersectionPoints: Set[Vector2] = getIntersectionPoints(wirePathSegments).toSet

    val fewestCombinedSteps: Int = intersectionPoints
      .map(
        (intersectionPoint: Vector2) => wirePathSegments.map(
            (pathSegments: Seq[PathSegment]) => stepsToReachPoint(intersectionPoint, pathSegments.iterator)
          )
          .reduce(_ + _)
      )
      .reduce(min)

    println(s"The fewest combined steps the wires must take to reach an intersection is ${fewestCombinedSteps}.")
  }
}
