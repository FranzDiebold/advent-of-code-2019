/**
 * Intcode Program computer
 */
package day09

import scala.io.Source

class UnknownOpcode extends Exception
class UnknownParameterMode extends Exception

object IntcodeProgram {
  type Program = Map[BigInt, BigInt]
  type Modes = IndexedSeq[Int]
  type ProgramOutput = Seq[BigInt]

  val OpcodeAdd = 1
  val OpcodeMultiply = 2
  val OpcodeInput = 3
  val OpcodeOutput = 4
  val OpcodeJumpIfTrue = 5
  val OpcodeJumpIfFalse = 6
  val OpcodeLessThan = 7
  val OpcodeEquals = 8
  val OpcodeAdjustRelativeBase = 9
  val OpcodeHalt = 99

  val ParameterPositionMode = 0
  val ParameterImmediateMode = 1
  val ParameterRelativeMode = 2

  @throws(classOf[UnknownParameterMode])
  def getParameter(program: Program, relativeBase: BigInt, idx: BigInt, mode: Int): BigInt = {
    mode match {
      case ParameterPositionMode => program(idx)
      case ParameterImmediateMode => idx
      case ParameterRelativeMode => relativeBase + program(idx)
      case _ => throw new UnknownParameterMode
    }
  }

  def binaryOperation(program: Program, relativeBase: BigInt, idx: BigInt, modes: Modes, operation: (BigInt, BigInt) => BigInt): Program = {
    val value: BigInt = operation(
        program(getParameter(program, relativeBase, idx, modes(0))),
        program(getParameter(program, relativeBase, idx + 1, modes(1))))
    program + (getParameter(program, relativeBase, idx + 2, modes(2)) -> value)
  }

  def inputOperation(program: Program, relativeBase: BigInt, idx: BigInt, modes: Modes, inputValue: BigInt): Program = {
    program + (getParameter(program, relativeBase, idx, modes(0)) -> inputValue)
  }

  def outputOperation(program: Program, relativeBase: BigInt, idx: BigInt, modes: Modes): BigInt = program(getParameter(program, relativeBase, idx, modes(0)))

  def jumpIf(program: Program, relativeBase: BigInt, idx: BigInt, modes: Modes, condition: Boolean): BigInt = {
    val conditionValue: BigInt = program(getParameter(program, relativeBase, idx, modes(0)))
    if ((conditionValue != 0 && condition) || (conditionValue == 0 && !condition)) {
      program(getParameter(program, relativeBase, idx + 1, modes(1)))
    } else {
      idx + 2
    }
  }

  def compare(program: Program, relativeBase: BigInt, idx: BigInt, modes: Modes, lessThan: Boolean): Program = {
    val parameter1: BigInt = program(getParameter(program, relativeBase, idx, modes(0)))
    val parameter2: BigInt = program(getParameter(program, relativeBase, idx + 1, modes(1)))
    val value: BigInt = {
      if ((parameter1 < parameter2 && lessThan) || (parameter1 == parameter2 && !lessThan)) {
        1
      } else {
        0
      }
    }
    program + (getParameter(program, relativeBase, idx + 2, modes(2)) -> value)
  }

  def adjustRelativeBase(program: Program, relativeBase: BigInt, idx: BigInt, modes: Modes): BigInt = {
    relativeBase + program(getParameter(program, relativeBase, idx, modes(0)))
  }

  @throws(classOf[UnknownOpcode])
  @throws(classOf[UnknownParameterMode])
  def runProgram(program: Program, inputIter: Iterator[BigInt], relativeBase: BigInt = 0, instructionPointer: BigInt = 0, output: ProgramOutput = List()): ProgramOutput = {
    val instruction: String = f"${program(instructionPointer)}%05d"
    val opcode: Int = instruction.substring(3).toInt
    val modes: Modes = instruction
      .substring(0, 3)
      .map(_.asDigit)
      .reverse
    opcode match {
      case OpcodeHalt => output
      case OpcodeAdd => runProgram(binaryOperation(program, relativeBase, instructionPointer + 1, modes, _ + _), inputIter, relativeBase, instructionPointer + 4, output)
      case OpcodeMultiply => runProgram(binaryOperation(program, relativeBase, instructionPointer + 1, modes, _ * _), inputIter, relativeBase, instructionPointer + 4, output)
      case OpcodeInput => runProgram(inputOperation(program, relativeBase, instructionPointer + 1, modes, inputIter.next()), inputIter, relativeBase, instructionPointer + 2, output)
      case OpcodeOutput => runProgram(program, inputIter, relativeBase, instructionPointer + 2, output :+ outputOperation(program, relativeBase, instructionPointer + 1, modes))
      case OpcodeJumpIfTrue => runProgram(program, inputIter, relativeBase, jumpIf(program, relativeBase, instructionPointer + 1, modes, true), output)
      case OpcodeJumpIfFalse => runProgram(program, inputIter, relativeBase, jumpIf(program, relativeBase, instructionPointer + 1, modes, false), output)
      case OpcodeLessThan => runProgram(compare(program, relativeBase, instructionPointer + 1, modes, true), inputIter, relativeBase, instructionPointer + 4, output)
      case OpcodeEquals => runProgram(compare(program, relativeBase, instructionPointer + 1, modes, false), inputIter, relativeBase, instructionPointer + 4, output)
      case OpcodeAdjustRelativeBase => runProgram(program, inputIter, adjustRelativeBase(program, relativeBase, instructionPointer + 1, modes), instructionPointer + 2, output)
      case _ => throw new UnknownOpcode
    }
  }

  def readProgramFromFile(filePath: String): Program = {
    Source
      .fromFile(filePath)
      .getLines
      .next()
      .split(',')
      .map(_.trim)
      .map(BigInt(_))
      .zipWithIndex
      .foldLeft(Map[BigInt, BigInt]().withDefaultValue(BigInt(0)))(
        (program: Program, valueWithIndex: Tuple2[BigInt, Int]) => program + (BigInt(valueWithIndex._2) -> valueWithIndex._1)
      )
  }
}
