package matchcomputer

object Command {

  def inc(registerNumber: RegisterNumber): CommandF =
    incDecF(Registers.inc(Registers.apply)(registerNumber))

  def dec(registerNumber: RegisterNumber): CommandF =
    incDecF(Registers.dec(Registers.apply)(registerNumber))

  def jmp(lineNumber: LineNumber): CommandF =
    (registers: Registers) => Right((registers, ContinueWithLine(lineNumber)))

  def isz(registerNumber: RegisterNumber): CommandF =
    iszF(Registers.isZero(registerNumber))

  def sub(program: Program): CommandF = ???

  def stp: CommandF = (registers: Registers) => Right((registers, Stop))

  //// Internal commands
  def incDecF(rs2morsF: Registers2MorRegistersF): CommandF =
    rs2morsF.andThen((mors: MorRegisters) =>
      mors.map((rs: Registers) => (rs, ContinueWithNextLine)))

  def iszF(rs2morb: Registers => MorBoolean): CommandF =
    (registers: Registers) =>
      rs2morb(registers)
        .map(
          (zero: Boolean) =>
            (registers,
              if (zero) ContinueWithSecondNextLine else ContinueWithNextLine))

}
