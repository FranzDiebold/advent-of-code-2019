/**
 * Intcode Program computer
 */

package day05

class UnknownOpcode extends Exception
class UnknownParameterMode extends Exception

object IntcodeProgram {
  type Program = Array[Int]
  type Modes = IndexedSeq[Int]
  type ProgramOutput = Seq[Int]

  val OpcodeAdd = 1
  val OpcodeMultiply = 2
  val OpcodeInput = 3
  val OpcodeOutput = 4
  val OpcodeJumpIfTrue = 5
  val OpcodeJumpIfFalse = 6
  val OpcodeLessThan = 7
  val OpcodeEquals = 8
  val OpcodeHalt = 99

  val ParameterPositionMode = 0
  val ParameterImmediateMode = 1

  @throws(classOf[UnknownParameterMode])
  def getParameter(program: Program, idx: Int, mode: Int): Int = {
    mode match {
      case ParameterPositionMode => program(idx)
      case ParameterImmediateMode => idx
      case _ => throw new UnknownParameterMode
    }
  }

  def binaryOperation(program: Program, idx: Int, modes: Modes, operation: (Int, Int) => Int): Program = {
    program(getParameter(program, idx + 2, modes(2))) =
      operation(
        program(getParameter(program, idx, modes(0))),
        program(getParameter(program, idx + 1, modes(1))))
    program
  }

  def inputOperation(program: Program, idx: Int, modes: Modes, inputValue: Int): Program = {
    program(getParameter(program, idx, modes(0))) = inputValue
    program
  }

  def outputOperation(program: Program, idx: Int, modes: Modes): Int = program(getParameter(program, idx, modes(0)))

  def jumpIf(program: Program, idx: Int, modes: Modes, condition: Boolean): Int = {
    val conditionValue: Int = program(getParameter(program, idx, modes(0)))
    if ((conditionValue != 0 && condition) || (conditionValue == 0 && !condition)) {
      program(getParameter(program, idx + 1, modes(1)))
    } else {
      idx + 2
    }
  }

  def compare(program: Program, idx: Int, modes: Modes, lessThan: Boolean): Program = {
    val parameter1: Int = program(getParameter(program, idx, modes(0)))
    val parameter2: Int = program(getParameter(program, idx + 1, modes(1)))
    program(getParameter(program, idx + 2, modes(2))) = {
      if ((parameter1 < parameter2 && lessThan) || (parameter1 == parameter2 && !lessThan)) {
        1
      } else {
        0
      }
    }
    program
  }

  @throws(classOf[UnknownOpcode])
  @throws(classOf[UnknownParameterMode])
  def runProgram(program: Program, inputIter: Iterator[Int], instructionPointer: Int = 0, output: ProgramOutput = List()): ProgramOutput = {
    val instruction: String = f"${program(instructionPointer)}%05d"
    val opcode: Int = instruction.substring(3).toInt
    val modes: Modes = instruction
      .substring(0, 3)
      .map(_.asDigit)
      .reverse
    opcode match {
      case OpcodeHalt => output
      case OpcodeAdd => runProgram(binaryOperation(program, instructionPointer + 1, modes, _ + _), inputIter, instructionPointer + 4, output)
      case OpcodeMultiply => runProgram(binaryOperation(program, instructionPointer + 1, modes, _ * _), inputIter, instructionPointer + 4, output)
      case OpcodeInput => runProgram(inputOperation(program, instructionPointer + 1, modes, inputIter.next()), inputIter, instructionPointer + 2, output)
      case OpcodeOutput => runProgram(program, inputIter, instructionPointer + 2, output :+ outputOperation(program, instructionPointer + 1, modes))
      case OpcodeJumpIfTrue => runProgram(program, inputIter, jumpIf(program, instructionPointer + 1, modes, true), output)
      case OpcodeJumpIfFalse => runProgram(program, inputIter, jumpIf(program, instructionPointer + 1, modes, false), output)
      case OpcodeLessThan => runProgram(compare(program, instructionPointer + 1, modes, true), inputIter, instructionPointer + 4, output)
      case OpcodeEquals => runProgram(compare(program, instructionPointer + 1, modes, false), inputIter, instructionPointer + 4, output)
      case _ => throw new UnknownOpcode
    }
  }
}
