/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 6: Universal Orbit Map
 * Part 1
 */
package day06

import scala.collection.mutable.Map
import scala.io.Source

import day06.EdgeUtility.{Edge, stringToEdge}

object Part1 {
  type Tree = Map[String, Seq[String]]

  def numberOfOrbitsTraversal(tree: Tree, currentNode: String, currentDepth: Int = 0): Int = {
    if (tree(currentNode).isEmpty) {
      currentDepth
    } else {
      currentDepth + tree(currentNode)
        .map((childNode: String) => numberOfOrbitsTraversal(tree, childNode, currentDepth + 1))
        .reduce(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    val edges: Iterator[Edge] = Source
      .fromFile("day06/input.txt")
      .getLines
      .map(stringToEdge)

    val tree: Tree = Map[String, Seq[String]]().withDefaultValue(Seq[String]())
    edges
      .foreach((edge: Edge) => {
        tree(edge._1) = tree(edge._1) :+ edge._2
      })

    val rootNode = "COM"

    val totalNumberOfOrbits: Int = numberOfOrbitsTraversal(tree, rootNode)

    println(s"The total number of direct and indirect orbits is ${totalNumberOfOrbits}.")
  }
}
