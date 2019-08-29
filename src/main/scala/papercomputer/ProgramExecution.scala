package papercomputer

import scala.annotation.tailrec

object ProgramExecution {
  lazy val execute: ProgramExecution =
    executeObserved(_ => (), ProgramState.next)

  def executeObserved(beforeNextF: ProgramState => Unit,
                      next: ProgramState => Mor[ProgramState] = ProgramState.next)
    : ProgramExecution =
    (program: Program, registers: Registers) =>
      nextTailRec(beforeNextF, next, ProgramState(program, registers))

  @tailrec
  def nextTailRec(beforeNextF: ProgramState => Unit,
                  next: ProgramState => Mor[ProgramState],
                  morPs: Mor[ProgramState]): Mor[Registers] = {
    morPs match {
      case Left(message)                        => Left(message)
      case Right(ps) if ps.currentLineO.isEmpty => Right(ps.registers)
      case Right(ps) =>
        beforeNextF(ps)
        nextTailRec(beforeNextF, next, next(ps))
    }
  }
}
