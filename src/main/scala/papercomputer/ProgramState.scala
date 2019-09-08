package papercomputer

import cats.data.{Kleisli, StateT}
import cats.implicits.{catsStdInstancesForEither, toFunctorOps}
import cats.{Applicative, Id}
import fs2.{Pure, Stream}

import scala.annotation.tailrec

/**
  * @param registersOpsConfig The configuration of the linkage between Command and Registers.
  * @param program      The immutable Program of the currently active stack, will not change during runtime of this stack.
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
final case class ProgramState private (registersOpsConfig: RegistersOpsConfig,
                                       program: Program,
                                       stack: Stack,
                                       currentLineO: Option[LineNumber],
                                       registers: Registers) {

  // override visibility and functionality of case class copy, see
  // https://blog.leifbattermann.de/2017/04/26/a-new-scala-feature-for-making-illegal-states-unrepresentable/#more-1255
  // this is not private as else we get an "method unused" warning
  def copy(): Unit = ()
}

object ProgramState {

  def apply(registersOpsConfig: RegistersOpsConfig,
            program: Program,
            stack: Stack,
            currentLineO: Option[LineNumber],
            registers: Registers): Mor[ProgramState] =
    if (currentLineO.isEmpty && stack.nonEmpty) Left(IllegalState)
    else if (!currentLineO.forall(program.contains))
      Left(StartLineNotFoundInProgram)
    else if (!checkStack(stack))
      Left(IllegalReferenceToNonExistingLineNumber)
    else
      Right(
        new ProgramState(registersOpsConfig,
                         program,
                         stack,
                         currentLineO,
                         registers))

  private def apply(program: Program, registers: Registers): Mor[ProgramState] =
    apply(
      RegistersOpsConfig(RegistersOps.inc, RegistersOps.dec, RegistersOps.isz),
      program,
      emptyStack,
      startLineO(program),
      registers
    )

  val morPsFromProgramStateConfig
    : Kleisli[Mor, ProgramStateConfig, ProgramState] =
    for {
      registers <- Registers.fromRegistersConfig
        .local[ProgramStateConfig](_.registersConfig)
      programState <- Kleisli[Mor, ProgramStateConfig, ProgramState](
        programStateConfig => apply(programStateConfig.program, registers))
    } yield programState

  val nextPs: ProgramState => Mor[ProgramState] =
    (currentProgramState: ProgramState) => {
      val registersOpsConfig = currentProgramState.registersOpsConfig
      val program = currentProgramState.program
      val stack = currentProgramState.stack
      val currentLineO = currentProgramState.currentLineO
      val registers = currentProgramState.registers

      lazy val currentLine: LineNumber = currentLineO.get

      def nextLineNumberFromGivenLine(
          currentLn: LineNumber): Mor[LineNumber] = {
        val (nextLines, _) = program.keySet.partition(_.value > currentLn.value)
        if (nextLines.nonEmpty) Right(nextLines.minBy(_.value))
        else Left(NoNextLinenNumberFoundInProgram)
      }

      lazy val nextLineNumberFromCurrentLine: Mor[LineNumber] =
        nextLineNumberFromGivenLine(currentLine)

      def continueWithNextLine(newRegisters: Registers): Mor[ProgramState] =
        nextLineNumberFromCurrentLine.flatMap(ln =>
          continueWithGivenLine(ln, newRegisters))

      def continueWithGivenLine(
          newLineNumber: LineNumber,
          newRegisters: Registers = registers): Mor[ProgramState] =
        continueWithProgram(program, stack, Some(newLineNumber), newRegisters)

      def continueWithProgram(
          newProgram: Program,
          newStack: Stack,
          newLineNumberO: Option[LineNumber],
          newRegisters: Registers = registers): Mor[ProgramState] =
        ProgramState(registersOpsConfig,
                     newProgram,
                     newStack,
                     newLineNumberO,
                     newRegisters)

      def prgSubNextProgramState(
          programToStack: Program,
          programToExecute: Program,
          newCurrentLineO: Option[LineNumber]): Mor[ProgramState] =
        for {
          nextLine <- nextLineNumberFromCurrentLine
          newStack = (programToStack, nextLine) +: stack
          programState <- continueWithProgram(programToExecute,
                                              newStack,
                                              newCurrentLineO)
        } yield programState

      def incDec(incDecF: IncDecF,
                 registerNumber: RegisterNumber): Mor[ProgramState] =
        incDecF(registerNumber)(registers).flatMap(continueWithNextLine)

      def jmp(jmpLine: LineNumber): Mor[ProgramState] =
        if (program.contains(jmpLine)) continueWithGivenLine(jmpLine)
        else Left(IllegalReferenceToNonExistingLineNumber)

      def isz(registerNumber: RegisterNumber): Mor[ProgramState] =
        for {
          zero <- registersOpsConfig.iszF(registerNumber)(registers)
          sameOrFirstNextLine <- if (zero) nextLineNumberFromCurrentLine
          else Right(currentLine)
          firstOrSecondNextLine <- nextLineNumberFromGivenLine(
            sameOrFirstNextLine)
          programState <- continueWithGivenLine(firstOrSecondNextLine)
        } yield programState

      def stp(): Mor[ProgramState] = {
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
      def prg(subProgram: Program): Mor[ProgramState] =
        if (subProgram.isEmpty) continueWithNextLine(registers)
        else
          prgSubNextProgramState(programToStack = program,
                                 programToExecute = subProgram,
                                 newCurrentLineO = startLineO(subProgram))

      // Sub first checks if a next line is available and only if so continues with the sub program,
      // That is: if there is no next line, the sub is not executed
      def sub(subLine: LineNumber): Mor[ProgramState] =
        if (!program.contains(subLine))
          Left(IllegalReferenceToNonExistingLineNumber)
        else
          prgSubNextProgramState(programToStack = program,
                                 programToExecute = program,
                                 newCurrentLineO = Some(subLine))

      currentLineO.map(program) match {
        case None => Left(CannotRunAFinishedProgram)
        case Some(Inc(registerNumber)) =>
          incDec(registersOpsConfig.incF, registerNumber)
        case Some(Dec(registerNumber)) =>
          incDec(registersOpsConfig.decF, registerNumber)
        case Some(Jmp(jmpLine))        => jmp(jmpLine)
        case Some(Isz(registerNumber)) => isz(registerNumber)
        case Some(Stp)                 => stp()
        case Some(Prg(subProgram))     => prg(subProgram)
        case Some(Sub(subLine))        => sub(subLine)
      }
    }

  /** Given a f to transform A to F[A], returns a StateT which this effect F and the Tuple2 of next state and current
    * state.
    */
  def nextAndCurrentState[A, F[_]: Applicative](f: A => F[A]): StateT[F, A, A] =
    StateT(current => f(current).map((_, current)))

  /** ProgramState => Message or Tuple2 of (next ProgramState, current ProgramState)
    * If the current ProgramState's currentLineO is a Some,
    * this returns the ProgramState after execution of the currentLine's Command,
    * or a failure Message.
    * If currentLineO is a None, this returns a failure message.
    */
  val next: StateT[Mor, ProgramState, ProgramState] = nextAndCurrentState(
    nextPs)

  /** Converts a ProgramState to a (potentially infinite) stream. */
  @tailrec
  def stream(streamSoFar: Stream[Pure, Mor[ProgramState]],
             morPs: Mor[ProgramState],
             nextF: ProgramState => Mor[ProgramState])
    : Stream[Pure, Mor[ProgramState]] =
    morPs match {
      case Left(_) => streamSoFar ++ Stream.emit(morPs)
      case Right(ps) =>
        val newStream = streamSoFar ++ Stream.emit(morPs)
        if (ps.currentLineO.isEmpty) newStream
        else stream(newStream, nextF(ps), nextF)
    }

  val streamFromProgramStateConfig
    : Kleisli[Id, ProgramStateConfig, Stream[Pure, Mor[ProgramState]]] =
    morPsFromProgramStateConfig.mapF[Id, Stream[Pure, Mor[ProgramState]]](
      stream(Stream.empty, _, nextPs))

  private def checkStack(stack: Stack): Boolean = stack.forall {
    case (p, ln) => p.contains(ln)
  }

  private def startLineO(program: Program): Option[LineNumber] =
    if (program.isEmpty) None else Some(program.keySet.minBy(_.value))
}

final case class ProgramStateConfig(program: Program,
                                    registersConfig: RegistersConfig)

// The linkage between Command and Registers
final case class RegistersOpsConfig(incF: IncDecF, decF: IncDecF, iszF: IszF)
