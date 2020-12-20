package papercomputer

import cats.data.Kleisli

final case class Registers private(minRegisterValue: RegisterValue,
                                   maxRegisterValue: RegisterValue,
                                   registerValues: RegisterValues) {
  // override visibility and functionality of case class copy, see
  // https://blog.leifbattermann.de/2017/04/26/a-new-scala-feature-for-making-illegal-states-unrepresentable/#more-1255
  // this is not private as else we get an "method unused" warning
  def copy(): Unit = ()
}

object Registers {
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

  val fromRegistersConfig: Kleisli[Mor, RegistersConfig, Registers] = Kleisli(
    (registersConfig: RegistersConfig) =>
      apply(registersConfig.minRegisterValue,
        registersConfig.maxRegisterValue,
        registersConfig.registerValues))
}

object RegistersOps {
  val inc: IncDecF = (registerNumber: RegisterNumber) =>
    (registers: Registers) =>
      incDec(registerNumber,
        registers,
        valueToWrap = registers.maxRegisterValue,
        wrappedValue = registers.minRegisterValue,
        newValueF = (v: RegisterValue) => v + 1)

  val dec: IncDecF =
    (registerNumber: RegisterNumber) =>
      (registers: Registers) =>
        incDec(registerNumber,
          registers,
          valueToWrap = registers.minRegisterValue,
          wrappedValue = registers.maxRegisterValue,
          newValueF = (v: RegisterValue) => v - 1)

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
      newConfig = RegistersConfig(registers.minRegisterValue,
        registers.maxRegisterValue,
        rvsNew)
      rsNew <- Registers.fromRegistersConfig(newConfig)
    } yield rsNew

  private def registerValue(registerNumber: RegisterNumber,
                            registers: Registers): Mor[RegisterValue] =
    registers.registerValues
      .get(registerNumber)
      .toRight(IllegalAccessToNonExistingRegisterNumber)

}

final case class RegistersConfig(minRegisterValue: RegisterValue,
                                 maxRegisterValue: RegisterValue,
                                 registerValues: RegisterValues)
