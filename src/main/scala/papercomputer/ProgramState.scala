package papercomputer

import cats.data.StateT
import cats.implicits._

/**
  *
  * @param program The immutable Program of the currently active stack, will not change during runtime of this stack.
  * @param stack        A stack to keep track of the next program and line numbers where to continue after a Stp.
  *                     Initially, this list is empty.
  *                     During execution of a Prg(subPrg), the current program and the line where to continue in the
  *                     current program after the Prg command finished is pushed to the head of the stack.
  *                     During execution of a Sub(subLine), the current program and the line where to continue in the
  *                     current program after the Sub finished is pushed to the head of the stack.
  *                     When executing a Stp the first entry is popped to continue after the Prg or Sub.
  *                     If this list is empty and a Stp is executed, the currentLineO turns to None to indicate the
  *                     program is finished.
  * @param currentLineO A Some(LineNumber) indicated the LineNumber of the current not yet-executed Command,
  *                     guaranteed to be included in the program.
  *                     A None indicated that the program execution is finished.
  * @param registers    The registers in the state before the execution of the Command in currentLineO.
  */
final case class ProgramState private (program: Program,
                                       stack: Stack,
                                       currentLineO: Option[LineNumber],
                                       registers: Registers) {

  // override visibility and functionality of case class copy, see
  // https://blog.leifbattermann.de/2017/04/26/a-new-scala-feature-for-making-illegal-states-unrepresentable/#more-1255
  // this is not private as else we get an "method unused" warning
  def copy(): Unit = ()
}

object ProgramState {
  def apply(program: Program, registers: Registers): MorProgramState =
    ProgramState(program, emptyStack, startLineO(program), registers)

  def apply(program: Program,
            stack: Stack,
            currentLineO: Option[LineNumber],
            registers: Registers): MorProgramState =
    if (currentLineO.isEmpty && stack.nonEmpty) Left(IllegalState)
    else if (!currentLineO.forall(program.contains))
      Left(StartLineNotFoundInProgram)
    else if (!checkStack(stack))
      Left(IllegalReferenceToNonExistingLineNumber)
    else
      Right(new ProgramState(program, stack, currentLineO, registers))

  /** ProgramState => Message or next ProgramState
    * If the current ProgramState's currentLineO is a Some,
    * this returns the ProgramState after execution of the currentLine's Command,
    * or a failure Message.
    * If currentLineO is a None, this returns a failure message.
    * It needs the functions IncF, DecF and IszF as parameters.
    */
  def next(incF: IncF,
           decF: DecF,
           iszF: IszF): ProgramState => MorProgramState =
    nextProgramStateUnit(incF, decF, iszF).runS

  def nextProgramStateUnit(incF: IncF,
                           decF: DecF,
                           iszF: IszF): StateT[Mor, ProgramState, Unit] =
    nextProgramStateT[Unit](incF, decF, iszF)(_ => ())

  /** ProgramState => Message or Tuple2 of (next ProgramState, result of f on next ProgramState) */
  def nextProgramStateT[T](incF: IncF, decF: DecF, iszF: IszF)(
      f: ProgramState => T): StateT[Mor, ProgramState, T] =
    StateT { currentProgramState =>
      val program = currentProgramState.program
      val stack = currentProgramState.stack
      val currentLineO = currentProgramState.currentLineO
      val registers = currentProgramState.registers

      lazy val currentLine: LineNumber = currentLineO.get

      lazy val nextLineNumberFromCurrentLine: Mor[LineNumber] =
        nextLineNumberFromGivenLine(currentLine)

      def nextLineNumberFromGivenLine(
          currentLn: LineNumber): Mor[LineNumber] = {
        val (nextLines, _) = program.keySet.partition(_.value > currentLn.value)
        if (nextLines.nonEmpty) Right(nextLines.minBy(_.value))
        else Left(NoNextLinenNumberFoundInProgram)
      }

      def continueWithNextLine(newRegisters: Registers): MorProgramState =
        nextLineNumberFromCurrentLine.flatMap(ln =>
          continueWithLine(ln, newRegisters))

      def continueWithLine(
          newLineNumber: LineNumber,
          newRegisters: Registers = registers): MorProgramState =
        continueWithProgram(program, stack, Some(newLineNumber), newRegisters)

      def continueWithProgram(
          newProgram: Program,
          newStack: Stack,
          newLineNumberO: Option[LineNumber],
          newRegisters: Registers = registers): MorProgramState =
        ProgramState(newProgram, newStack, newLineNumberO, newRegisters)

      def prgSubNextProgramState(
          programToStack: Program,
          programToExecute: Program,
          newCurrentLineO: Option[LineNumber]): MorProgramState =
        for {
          nextLine <- nextLineNumberFromCurrentLine
          newStack = (programToStack, nextLine) +: stack
          programState <- continueWithProgram(programToExecute,
                                              newStack,
                                              newCurrentLineO)
        } yield programState

      def incDec(incDecF: IncDecF,
                 registerNumber: RegisterNumber): MorProgramState =
        incDecF(registerNumber)(registers).flatMap(continueWithNextLine)

      def jmp(jmpLine: LineNumber): MorProgramState =
        if (program.contains(jmpLine)) continueWithLine(jmpLine)
        else Left(IllegalReferenceToNonExistingLineNumber)

      def isz(registerNumber: RegisterNumber): MorProgramState =
        for {
          zero <- iszF(registerNumber)(registers)
          sameOrFirstNextLine <- if (zero) nextLineNumberFromCurrentLine
          else Right(currentLine)
          firstOrSecondNextLine <- nextLineNumberFromGivenLine(
            sameOrFirstNextLine)
          programState <- continueWithLine(firstOrSecondNextLine)
        } yield programState

      def stp(): MorProgramState = {
        val (newProgram, newCurrentLineO) = stack
          .map({ case (program, currentLine) => (program, Some(currentLine)) })
          .headOption
          .getOrElse((program, None))
        val newStack = stack.drop(1)
        continueWithProgram(newProgram, newStack, newCurrentLineO)
      }

      // Prg first checks if a next line is available and only if so continues with the sub program,
      // That is: if there is no next line, the subPrg is not executed
      // If the subprogram is empty, it is skipped
      def prg(subProgram: Program): MorProgramState =
        if (subProgram.isEmpty) continueWithNextLine(registers)
        else
          prgSubNextProgramState(programToStack = program,
                                 programToExecute = subProgram,
                                 newCurrentLineO = startLineO(subProgram))

      // Sub first checks if a next line is available and only if so continues with the sub program,
      // That is: if there is no next line, the sub is not executed
      def sub(subLine: LineNumber): MorProgramState =
        if (!program.contains(subLine))
          Left(IllegalReferenceToNonExistingLineNumber)
        else
          prgSubNextProgramState(programToStack = program,
                                 programToExecute = program,
                                 newCurrentLineO = Some(subLine))

      for {
        newPs <- currentLineO.map(program) match {
          case None                      => Left(CannotRunAFinishedProgram)
          case Some(Inc(registerNumber)) => incDec(incF, registerNumber)
          case Some(Dec(registerNumber)) => incDec(decF, registerNumber)
          case Some(Jmp(jmpLine))        => jmp(jmpLine)
          case Some(Isz(registerNumber)) => isz(registerNumber)
          case Some(Stp)                 => stp()
          case Some(Prg(subProgram))     => prg(subProgram)
          case Some(Sub(subLine))        => sub(subLine)
        }
      } yield (newPs, f(newPs))
    }

  private def checkStack(stack: Stack): Boolean = stack.forall {
    case (p, ln) => p.contains(ln)
  }

  private def startLineO(program: Program): Option[LineNumber] =
    if (program.isEmpty) None else Some(program.keySet.minBy(_.value))

}