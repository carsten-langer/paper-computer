package papercomputer

import scala.annotation.tailrec

object ProgramExecution {
  /* todo StateT currently not needed
  /** State => (next state, next state) */
  def nextProgramState: StateT[Mor, ProgramState, ProgramState] =
    StateT[Mor, ProgramState, ProgramState](
      _.next.map(nextPs => (nextPs, nextPs)))

  /** State => Message or (next State, result of f on next State) */
  def nextProgramStateT[T](f: ProgramState => T): StateT[Mor, ProgramState, T] =
    nextProgramState.map(f(_))
   */

  lazy val execute: ProgramExecution = executeObserved(_ => ())

  def executeObserved(beforeNextF: ProgramState => Unit): ProgramExecution =
    (program: Program, registers: Registers) => {
      @tailrec
      def tailRec(morPs: MorProgramState): MorRegisters = {
        morPs match {
          case Left(message)                        => Left(message)
          case Right(ps) if ps.currentLineO.isEmpty => Right(ps.registers)
          case Right(ps) =>
            beforeNextF(ps)
            tailRec(
              ProgramState.next(Registers.inc(Registers.apply), // todo parameterize
                                Registers.dec(Registers.apply),
                                Registers.isZero)(ps))
        }
      }
      tailRec(ProgramState(program, registers))
    }
}
