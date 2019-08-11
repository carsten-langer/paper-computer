package matchcomputer

object ProgramExecution {

  def programExecution(p: Program, rs: Registers): MorRegisters = {
    if (p.isEmpty) Right(rs)
    else {
      val startLn: LineNumber = p.keys.minBy(_.value)
     ??? // programExecution(nextLineNumberFromCurrentLine, p, startLn, rs)
    }
  }

  def startLineNumber(linenumbers: LineNumbers): Option[LineNumber] =
    if (linenumbers.isEmpty) None else Some(linenumbers.minBy(_.value))

  def nextLineNumberFromCurrentLine(lineNumbers: LineNumbers,
                                    currentLn: LineNumber): Mor[LineNumber] = {
    val nextLines = lineNumbers.partition(_.value > currentLn.value)._1
    if (nextLines.isEmpty) Left(NoNextLinenumberFoundInProgram)
    else Right(nextLines.minBy(_.value))
  }

  def nextLineNumberFromProgramControl(
      lineNumbers: LineNumbers,
      currentLn: LineNumber,
      pc: ProgramControl): Mor[Option[LineNumber]] = pc match {
    case Stop => Right(None)
    case ContinueWithNextLine =>
      nextLineNumberFromCurrentLine(lineNumbers, currentLn).map(Some(_))
    case ContinueWithSecondNextLine =>
      nextLineNumberFromCurrentLine(lineNumbers, currentLn)
        .flatMap(nextLine =>
          nextLineNumberFromCurrentLine(lineNumbers, nextLine))
        .map(Some(_))
    case ContinueWithLine(nln) => Right(Some(nln))
  }

  // todo this is not yet unit-tested via property-style
  def commandFromLineNumber(p: Program, ln: LineNumber): Mor[CommandF] =
    p.get(ln).toRight(IllegalReferenceToNonexistingLinenumber)

  def programExecution(commandFromLineNumberF: (Program, LineNumber) => Mor[CommandF],
                       nextLineNumberFromProgramControlF: (LineNumbers, LineNumber, ProgramControl) => Mor[Option[LineNumber]],
                       p: Program,
                       ln: LineNumber,
                       rs: Registers): MorRegisters = {
    for {
      command <- commandFromLineNumberF(p, ln)
      newRsrc <- command(rs)
      (newRs, pc) = newRsrc
      lnO <- nextLineNumberFromProgramControlF(p.keySet, ln, pc)
      newRs <- lnO match {
        case None      => Right(newRs)
        case Some(nln) => programExecution(commandFromLineNumberF, nextLineNumberFromProgramControlF, p, nln, newRs)
      }
    } yield newRs
  }



}
