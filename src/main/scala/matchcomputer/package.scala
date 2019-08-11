import eu.timepit.refined.{refineV, refineMV}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.{NonNegative, Positive}
import scala.collection.immutable

package object matchcomputer {

  /** The architecture model of the match-computer determines the
    * - min and max register values,
    * - max register numbers
    * - max number of code lines in a program
    */
  type Value = Long
  val minValue: Value = Long.MinValue
  val maxValue: Value = Long.MaxValue

  type RegisterValue = Value
  val minRegisterValue: RegisterValue = minValue
  val maxRegisterValue: RegisterValue = maxValue

  type NonNegativeValue = Value Refined NonNegative
  type RegisterNumber = NonNegativeValue
  val minRegisterNumber: RegisterNumber = refineMV[NonNegative](0)
  val maxRegisterNumber: RegisterNumber =
    refineV[NonNegative](maxValue).right.get

  type Mor[T] = Either[Message, T]
  type MorBoolean = Mor[Boolean]
  type MorRegisterValue = Mor[RegisterValue]
  type MorRegistersProgramControl = Mor[(Registers, ProgramControl)]

  type RegisterValues = immutable.Map[RegisterNumber, RegisterValue]
  type MorRegisters = Mor[Registers]
  type RegistersFactory =
    (RegisterValue, RegisterValue, RegisterValues) => MorRegisters
  type Registers2MorRegistersF = Registers => MorRegisters

  type LineNumber = Value Refined Positive
  val minLineNumber: LineNumber = refineMV[Positive](1)
  val maxLineNumber: LineNumber = refineV[Positive](maxValue).right.get
  type LineNumbers = Set[LineNumber]
  type CommandF = Registers => MorRegistersProgramControl
  type Program = immutable.Map[LineNumber, CommandF]

  /** Executes a program on registers starting with the lowest line number. */
  type ProgramExecutionF = (Program, Registers) => MorRegisters
}
