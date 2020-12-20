import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{NonNegative, Positive}
import eu.timepit.refined.refineMV

import scala.collection.immutable

package object papercomputer {

  // TODO reword match-computer to paper-computer throughout the project

  /** The architecture model of the match-computer determines independently the
    * - min and max register values,
    * - max register numbers (register numbers start at 0)
    * - max number of code lines in a program (code line numbers start at 1)
    */

  /** The range of values that a register can hold. */
  type RegisterValue = Long
  val minRegisterValue: RegisterValue = Long.MinValue
  val maxRegisterValue: RegisterValue = Long.MaxValue

  /** Register numbers are positive or 0. */
  type RegisterNumber = Long Refined NonNegative
  val minRegisterNumber: RegisterNumber = refineMV[NonNegative](0L)
  val maxRegisterNumber: RegisterNumber = refineMV[NonNegative](Long.MaxValue)

  /** A Message Or Either type used for message/error propagation. */
  type Mor[T] = Either[Message, T]

  /** A data structure to map register number to register value. */
  type RegisterValues = immutable.Map[RegisterNumber, RegisterValue]

  def emptyRegisterValues: RegisterValues = Map.empty

  /** Program line numbers are positive > 0. */
  type LineNumber = Long Refined Positive
  val minLineNumber: LineNumber = refineMV[Positive](1L)
  val maxLineNumber: LineNumber = refineMV[Positive](Long.MaxValue)
  type LineNumbers = Set[LineNumber]

  /** A stack to collect the program lines to return to after a sub program has finished. */
  type Stack = List[(Program, LineNumber)]

  def emptyStack: Stack = List.empty

  /** Functions to inc, dec, or test a given register number from given registers. */
  type IncDecF = RegisterNumber => Registers => Mor[Registers]
  type IszF = RegisterNumber => Registers => Mor[Boolean]

  /** In a program, each command has a line number. */
  type Program = immutable.Map[LineNumber, Command]

  def emptyProgram: Program = Map.empty

  /** Executes a program on registers, starting with the lowest line number.
    * Registers are build from a registers configuration.
    * Building the registers may fail with a message.
    * Executing the program may fail with a message.
    * Program execution may be infinite if program contains a loop.
    * If no failure and program finishes, the final registers are returned.
    */
  type ProgramExecutionF = (Program, RegistersConfig) => Mor[Registers]
}
