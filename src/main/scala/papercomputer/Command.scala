package papercomputer

sealed trait Command
sealed trait IncDec extends Command
case class Inc(registerNumber: RegisterNumber) extends IncDec
case class Dec(registerNumber: RegisterNumber) extends IncDec
case class Jmp(lineNumber: LineNumber) extends Command
case class Isz(registerNumber: RegisterNumber) extends Command
case class Prg(program: Program) extends Command
case class Sub(lineNumber: LineNumber) extends Command
case object Stp extends Command
