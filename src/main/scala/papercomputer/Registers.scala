package papercomputer

final case class Registers private (minRegisterValue: RegisterValue,
                                    maxRegisterValue: RegisterValue,
                                    registerValues: RegisterValues) {
  // override visibility and functionality of case class copy, see
  // https://blog.leifbattermann.de/2017/04/26/a-new-scala-feature-for-making-illegal-states-unrepresentable/#more-1255
  // this is not private as else we get an "method unused" warning
  def copy(): Unit = ()
}

object Registers {
  // Signature is equivalent to type RegistersFactory, but the implicit logic of Register() being equivalent to
  // Registers.apply() only works if apply is defined as a normal def with parameters.
  def apply(minRegisterValue: RegisterValue,
            maxRegisterValue: RegisterValue,
            registerValues: RegisterValues): MorRegisters = {
    if (minRegisterValue > 0)
      Left(MinRegisterValueMustBeLessOrEqualZero)
    else if (maxRegisterValue < 0)
      Left(MaxRegisterValueMustBeGreaterOrEqualZero)
    else if (registerValues.values.exists(_ < minRegisterValue))
      Left(RegisterValueMustNotBeSmallerThanMinRegisterValue)
    else if (registerValues.values.exists(_ > maxRegisterValue))
      Left(RegisterValueMustNotBeGreaterThanMaxRegisterValue)
    else
      Right(new Registers(minRegisterValue, maxRegisterValue, registerValues))
  }

  def isZero(registerNumber: RegisterNumber)(registers: Registers): MorBoolean =
    registerValue(registerNumber)(registers).map(_ == 0)

  def inc(registersFactory: RegistersFactory)(registerNumber: RegisterNumber)(
      registers: Registers): MorRegisters =
    incDec(registersFactory,
           registerNumber,
           registers,
           valueToWrap = registers.maxRegisterValue,
           wrappedValue = registers.minRegisterValue,
           newValueF = (v: Value) => v + 1)

  def dec(registersFactory: RegistersFactory)(registerNumber: RegisterNumber)(
      registers: Registers): MorRegisters =
    incDec(registersFactory,
           registerNumber,
           registers,
           valueToWrap = registers.minRegisterValue,
           wrappedValue = registers.maxRegisterValue,
           newValueF = (v: Value) => v - 1)

  // TODO decide if Registers.registerValue shall be in public API, currently it is private
  private def registerValue(registerNumber: RegisterNumber)(
      registers: Registers): MorRegisterValue =
    registers.registerValues
      .get(registerNumber)
      .toRight(IllegalAccessToNonExistingRegisterNumber)

  private def incDec(registersFactory: RegistersFactory,
                     registerNumber: RegisterNumber,
                     registers: Registers,
                     valueToWrap: RegisterValue,
                     wrappedValue: RegisterValue,
                     newValueF: RegisterValue => RegisterValue): MorRegisters =
    for {
      rv <- Registers.registerValue(registerNumber)(registers)
      rvNew = if (rv == valueToWrap) wrappedValue else newValueF(rv)
      rvsNew = registers.registerValues.updated(registerNumber, rvNew)
      rsNew <- registersFactory(registers.minRegisterValue,
                                registers.maxRegisterValue,
                                rvsNew)
    } yield rsNew
}
