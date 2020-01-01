/**
 * Advent of Code 2019
 * https://adventofcode.com/2019
 * Day 6: Universal Orbit Map
 * Part 2
 */
package day06

import scala.collection.mutable.Map
import scala.io.Source

import day06.EdgeUtility.{Edge, stringToEdge}

class NodesNotConnectedException extends Exception

object Part2 {
  type ParentTree = Map[String, String]

  def getHeightMap(parentTree: ParentTree, node: String, heightMap: Map[String, Int] = Map[String, Int](), height: Int = 0): Map[String, Int] = {
    if (!parentTree.contains(node)) {
      heightMap
    } else {
      val parent: String = parentTree(node)
      getHeightMap(parentTree, parent, heightMap ++ Map[String, Int](parent -> (height + 1)), height + 1)
    }
  }

  @throws(classOf[NodesNotConnectedException])
  def getNodeDistance(parentTree: ParentTree, heightMapA: Map[String, Int], nodeB: String, heightB: Int = 0): Int = {
    if (!parentTree.contains(nodeB)) {
      throw new NodesNotConnectedException
    } else {
      val parentB: String = parentTree(nodeB)
      if (heightMapA.contains(parentB)) {
        heightB + 1 + heightMapA(parentB)
      } else {
        getNodeDistance(parentTree, heightMapA, parentB, heightB + 1)
      }
    }
  }

  def getMinimumNumberOfOrbitalTransfers(parentTree: ParentTree, nodeA: String, nodeB: String): Int = {
    val heightMapA: Map[String, Int] = getHeightMap(parentTree, nodeA)
    getNodeDistance(parentTree, heightMapA, nodeB) - 2
  }

  def main(args: Array[String]): Unit = {
    val edges: Iterator[Edge] = Source
      .fromFile("day06/input.txt")
      .getLines
      .map(stringToEdge)

    val parentTree: ParentTree = Map[String, String]()
    edges
      .foreach((edge: Edge) => {
        parentTree(edge._2) = edge._1
      })

    val startNode = "YOU"
    val targetNode = "SAN"

    val minimumNumberOfOrbitalTransfers: Int = getMinimumNumberOfOrbitalTransfers(parentTree, startNode, targetNode)

    println(s"The minimum number of orbital transfers is ${minimumNumberOfOrbitalTransfers}.")
  }
}
