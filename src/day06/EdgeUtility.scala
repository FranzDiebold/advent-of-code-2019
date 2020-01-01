/**
 * Edge utility functions and types.
 */
package day06

import scala.util.matching.Regex

class InvalidEdgeException extends Exception

object EdgeUtility {
  type Edge = Tuple2[String, String]

  @throws(classOf[InvalidEdgeException])
  def stringToEdge(edge: String): Edge = {
    val edgePattern: Regex = "^([A-Z0-9]+)\\)([A-Z0-9]+)$".r
    edge match {
      case edgePattern(sourceNode, targetNode) => (sourceNode, targetNode)
      case _ => throw new InvalidEdgeException
    }
  }
}
