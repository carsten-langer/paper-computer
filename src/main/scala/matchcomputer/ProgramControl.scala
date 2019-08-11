package matchcomputer

sealed trait ProgramControl
case object Stop extends ProgramControl
case object ContinueWithNextLine extends ProgramControl
case object ContinueWithSecondNextLine extends ProgramControl
case class ContinueWithLine(lineNumber: LineNumber) extends ProgramControl
