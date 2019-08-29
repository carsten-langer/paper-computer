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
            registerValues: RegisterValues): Mor[Registers] = {
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
}

object RegistersOps {
  val inc: IncF = (registerNumber: RegisterNumber) =>
    (registers: Registers) =>
      incDec(registerNumber,
             registers,
             valueToWrap = registers.maxRegisterValue,
             wrappedValue = registers.minRegisterValue,
             newValueF = (v: Value) => v + 1)

  val dec: DecF =
    (registerNumber: RegisterNumber) =>
      (registers: Registers) =>
        incDec(registerNumber,
               registers,
               valueToWrap = registers.minRegisterValue,
               wrappedValue = registers.maxRegisterValue,
               newValueF = (v: Value) => v - 1)

  val isz: IszF = (registerNumber: RegisterNumber) =>
    (registers: Registers) =>
      registerValue(registerNumber, registers).map(_ == 0)

  private def incDec(registerNumber: RegisterNumber,
                     registers: Registers,
                     valueToWrap: RegisterValue,
                     wrappedValue: RegisterValue,
                     newValueF: RegisterValue => RegisterValue): Mor[Registers] =
    for {
      oldRv <- registerValue(registerNumber, registers)
      rvNew = if (oldRv == valueToWrap) wrappedValue else newValueF(oldRv)
      rvsNew = registers.registerValues.updated(registerNumber, rvNew)
      rsNew <- Registers.apply(registers.minRegisterValue,
                               registers.maxRegisterValue,
                               rvsNew)
    } yield rsNew

  private def registerValue(registerNumber: RegisterNumber,
                            registers: Registers): Mor[RegisterValue] =
    registers.registerValues
      .get(registerNumber)
      .toRight(IllegalAccessToNonExistingRegisterNumber)

}
