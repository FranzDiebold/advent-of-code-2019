/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 3: Crossed Wires
 * Part 1
 */
package day03

import scala.math.{abs, min}
import scala.io.Source

import day03.Wires.{Vector2, PathSegment, instructionsToPathSegments, getIntersectionPoints}

object Part1 {
  def manhattanDistance(v: Vector2): Int = abs(v._1) + abs(v._2)

  def main(args: Array[String]): Unit = {
    val wirePathSegments: Seq[Seq[PathSegment]] = Source
      .fromFile("day03/input.txt")
      .getLines
      .map(_.split(','))
      .map(instructionsToPathSegments)
      .toSeq

    val intersectionPoints: Seq[Vector2] = getIntersectionPoints(wirePathSegments)

    val closestIntersectionDistance: Int = intersectionPoints
      .map(manhattanDistance)
      .reduce(min)

    println(s"The Manhattan distance from the central port to the closest intersection is ${closestIntersectionDistance}.")
  }
}
