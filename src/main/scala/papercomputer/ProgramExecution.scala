package papercomputer

import cats.data.StateT
import cats.implicits._

object ProgramExecution {
  lazy val execute: ProgramExecutionF =
    executeObserved(_ => (), ProgramState.next)

  def executeObserved(sideEffect: => ProgramState => Unit,
                      next: StateT[Mor, ProgramState, ProgramState] =
                        ProgramState.next): ProgramExecutionF =
    (program: Program, registers: Registers) =>
      toStream(ProgramState(program, registers), next)
        .foldLeft[Mor[Registers]](Right(registers)) {
          case (_, morPs) =>
            for {
              ps <- morPs
              _ = sideEffect(ps)
            } yield ps.registers
      }

  def toStream(morPsPs: Mor[ProgramState],
               next: StateT[Mor, ProgramState, ProgramState])
    : Stream[Mor[ProgramState]] =
    morPsPs match {
      case Left(_) => Seq(morPsPs).toStream
      case Right(ps) if ps.currentLineO.isEmpty =>
        Seq(morPsPs).toStream
      case Right(ps) => Right(ps) #:: toStream(next.runS(ps), next)
    }

}
