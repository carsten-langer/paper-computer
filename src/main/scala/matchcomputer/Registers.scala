package matchcomputer

sealed trait Registers {
  def minRegisterValue: RegisterValue
  def maxRegisterValue: RegisterValue
  def registerValues: RegisterValues
}

object Registers {
  // equivalent to type RegistersFactory, but the implicit logic of Register() = Registers.apply() only works if
  // apply is defined as normal def with parameters
  def apply(minRegisterValue: RegisterValue,
            maxRegisterValue: RegisterValue,
            registerValues: RegisterValues): MorRegisters =
    RegistersImpl(minRegisterValue, maxRegisterValue, registerValues)

  def registerValue(registerNumber: RegisterNumber)(
      registers: Registers): MorRegisterValue =
    registers.registerValues
      .get(registerNumber)
      .toRight(IllegalAccessToNonexistingRegisterNumber)

  def isZero(registerNumber: RegisterNumber)(
      registers: Registers): MorBoolean =
    registerValue(registerNumber)(registers).map(_ == 0)

  def inc(registersFactory: RegistersFactory)(registerNumber: RegisterNumber)(
      registers: Registers): MorRegisters =
    incDec(registersFactory,
           registerNumber,
           registers,
           registers.maxRegisterValue,
           registers.minRegisterValue,
           (v: Value) => v + 1)

  def dec(registersFactory: RegistersFactory)(registerNumber: RegisterNumber)(
      registers: Registers): MorRegisters =
    incDec(registersFactory,
           registerNumber,
           registers,
           registers.minRegisterValue,
           registers.maxRegisterValue,
           (v: Value) => v - 1)

  private def incDec(
      registersFactory: RegistersFactory,
      registerNumber: RegisterNumber,
      registers: Registers,
      valueToWrap: RegisterValue,
      wrappedValue: RegisterValue,
      newValue: RegisterValue => RegisterValue): MorRegisters =
    for {
      rv <- Registers.registerValue(registerNumber)(registers)
      minRV = registers.minRegisterValue
      maxRV = registers.maxRegisterValue
      rvNew = if (rv == valueToWrap) wrappedValue else newValue(rv)
      rvsNew = registers.registerValues.updated(registerNumber, rvNew)
      rsNew <- registersFactory(minRV, maxRV, rvsNew)
    } yield rsNew

}

final case class RegistersImpl private (minRegisterValue: RegisterValue,
                                        maxRegisterValue: RegisterValue,
                                        registerValues: RegisterValues)
    extends Registers {
  // override visibility and functionality of case class copy, see
  // https://blog.leifbattermann.de/2017/04/26/a-new-scala-feature-for-making-illegal-states-unrepresentable/#more-1255
  // this is not private as else we get an "method unused" warning
  def copy(): Unit = ()
}

object RegistersImpl {
  def apply(minRegisterValue: RegisterValue,
            maxRegisterValue: RegisterValue,
            registerValues: RegisterValues): MorRegisters = {
    if (minRegisterValue > maxRegisterValue)
      Left(MinRegisterValueMustBeLessOrEqualMaxRegisterValue)
    else if (registerValues.exists(_._2 < minRegisterValue))
      Left(RegisterValueMustNotBeSmallerThanMinRegisterValue)
    else if (registerValues.exists(_._2 > maxRegisterValue))
      Left(RegisterValueMustNotBeGreaterThanMaxRegisterValue)
    else
      Right(
        new RegistersImpl(minRegisterValue, maxRegisterValue, registerValues))
  }
}
