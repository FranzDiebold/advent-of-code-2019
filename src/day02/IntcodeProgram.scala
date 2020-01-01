/**
 * Intcode Program computer
 */

package day02

class UnknownOpcode extends Exception

object IntcodeProgram {
  val OpcodeAdd = 1
  val OpcodeMultiply = 2
  val OpcodeHalt = 99

  def binaryOperation(program: Array[Int], idx: Int, operation: (Int, Int) => Int): Array[Int] = {
    program(program(idx + 2)) = operation(program(program(idx)), program(program(idx + 1)))
    program
  }

  @throws(classOf[UnknownOpcode])
  def runProgram(program: Array[Int], instructionPointer: Int = 0): Array[Int] = {
    program(instructionPointer) match {
      case OpcodeHalt => program
      case OpcodeAdd => runProgram(binaryOperation(program, instructionPointer + 1, _ + _), instructionPointer + 4)
      case OpcodeMultiply => runProgram(binaryOperation(program, instructionPointer + 1, _ * _), instructionPointer + 4)
      case _ => throw new UnknownOpcode
    }
  }
}
