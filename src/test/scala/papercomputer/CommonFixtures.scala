package papercomputer

import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric.{NonNegative, Positive}
import eu.timepit.refined.refineV
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck.{Arbitrary, Cogen, Gen}

trait CommonFixtures {

  def genRefinedFromValueRange[P](minValue: Value, maxValue: Value)(
      implicit v: Validate[Value, P]): Gen[Refined[Value, P]] =
    // for performance reasons this assumes only correct values are refined within these tests,
    // so refineV will always be a Right
    Gen.chooseNum(minValue, maxValue).map(refineV[P](_).right.get)

  def genMinMaxRegisterValue(lowestMinRv: RegisterValue = minRegisterValue,
                             highestMaxRv: RegisterValue = maxRegisterValue)
    : Gen[(RegisterValue, RegisterValue)] =
    for {
      minRv <- Gen.chooseNum[RegisterValue](lowestMinRv, 0)
      maxRv <- Gen.chooseNum[RegisterValue](0, highestMaxRv)
    } yield (minRv, maxRv)

  lazy val genRegisterNumber: Gen[RegisterNumber] =
    genRefinedFromValueRange[NonNegative](minRegisterNumber.value,
                                          maxRegisterNumber.value)

  def genMinMaxRegisterValueRegisterValues(
      minNumRegisters: RegisterNumber,
      lowestMinRv: RegisterValue = minRegisterValue,
      highestMaxRv: RegisterValue = maxRegisterValue)
    : Gen[(RegisterValue, RegisterValue, RegisterValues)] =
    for {
      (minRv, maxRv) <- genMinMaxRegisterValue(lowestMinRv, highestMaxRv)
      genRv = Gen.chooseNum(minRv, maxRv)
      genRnRv = Gen.zip(genRegisterNumber, genRv)
      rnRvs <- if (minNumRegisters.value == 0) Gen.mapOf(genRnRv)
      else Gen.nonEmptyMap(genRnRv).suchThat(_.size >= minNumRegisters.value)
    } yield (minRv, maxRv, rnRvs)

  def genMinMaxRegisterValueRegisterValuesRegisters(
      minNumRegisters: RegisterNumber,
      lowestMinRv: RegisterValue = minRegisterValue,
      highestMaxRv: RegisterValue = maxRegisterValue)
    : Gen[(RegisterValue, RegisterValue, RegisterValues, Registers)] =
    for {
      (minRV, maxRV, rvs) <- genMinMaxRegisterValueRegisterValues(
        minNumRegisters,
        lowestMinRv,
        highestMaxRv)
      rs = Registers(minRV, maxRV, rvs).right.get
    } yield (minRV, maxRV, rvs, rs)

  def genRegisters: Gen[Registers] =
    genMinMaxRegisterValueRegisterValuesRegisters(0L)
      .map(_._4)

  lazy val genLineNumber: Gen[LineNumber] =
    genRefinedFromValueRange[Positive](minLineNumber.value, maxLineNumber.value)

  lazy val genCommand: Gen[Command] = {
    val genInc = genRegisterNumber.map(Inc)
    val genDec = genRegisterNumber.map(Dec)
    val genJmp = genLineNumber.map(Jmp)
    val genIsz = genRegisterNumber.map(Isz)
    val genStp = Gen.const(Stp)
    Gen.oneOf(genInc, genDec, genJmp, genIsz, genStp)
  }

  def genProgramFromCommandlines(
      genSet: Gen[(LineNumber, Command)] => Gen[Set[(LineNumber, Command)]])
    : Gen[Program] =
    genSet(Gen.zip(genLineNumber, genCommand))
      .map(_.toMap)

  def genNonEmptyProgramMinLines(minProgLines: PosInt): Gen[Program] =
    genProgramFromCommandlines(
      Gen.nonEmptyContainerOf[Set, (LineNumber, Command)])
      .suchThat(_.size >= minProgLines.value)

  lazy val genNonEmptyProgram: Gen[Program] = genNonEmptyProgramMinLines(1)

  lazy val genProgramAndAnyContainedLine: Gen[(Program, LineNumber)] =
    for {
      program <- genNonEmptyProgram
      anyLine <- Gen.oneOf(program.keys.toSeq)
    } yield (program, anyLine)

  lazy val genMorRegisters: Gen[MorRegisters] = Gen.frequency(
    (1, Gen.const(Left(MessageDuringUnitTests))),
    (10, genRegisters.map(Right(_))))

  implicit lazy val coGenRegisters: Cogen[Registers] = Cogen(
    (rs: Registers) => rs.hashCode.toLong)

  lazy val genRsToMorRs: Gen[Registers => MorRegisters] =
    Gen.function1[Registers, MorRegisters](genMorRegisters)

  implicit lazy val coGenRegisterNumber: Cogen[RegisterNumber] = Cogen(
    (rn: RegisterNumber) => rn.hashCode.toLong)

  lazy val genIncDecF: Gen[IncDecF] =
    Gen.function1[RegisterNumber, Registers => MorRegisters](genRsToMorRs)

  lazy val genMorBoolean: Gen[MorBoolean] = Gen.frequency(
    (1, Left(MessageDuringUnitTests)),
    (10, Arbitrary.arbBool.arbitrary.map(Right(_))))

  lazy val genRsToMorB: Gen[Registers => MorBoolean] =
    Gen.function1[Registers, MorBoolean](genMorBoolean)

  lazy val genIszF: Gen[IszF] =
    Gen.function1[RegisterNumber, Registers => MorBoolean](genRsToMorB)

  lazy val genConfig: Gen[ProgramStateConfig] = for {
    incF <- genIncDecF
    decF <- genIncDecF
    iszF <- genIszF
  } yield ProgramStateConfig(incF, decF, iszF)

  lazy val genStack: Gen[Stack] = Gen.listOf(genProgramAndAnyContainedLine)

  // create a new ProgramState under the assumption that all parameters are ok,
  // so it will always be a Right
  def newProgramState(config: ProgramStateConfig,
                      program: Program,
                      stack: Stack,
                      currentLineO: Option[LineNumber],
                      rs: Registers): ProgramState =
    ProgramState(config, program, stack, currentLineO, rs).right.get

  def genProgramState(program: Program,
                      stack: Stack,
                      currentLine: LineNumber): Gen[ProgramState] =
    for {
      config <- genConfig
      registers <- genRegisters
    } yield
      newProgramState(config, program, stack, Some(currentLine), registers)

  def genProgramState(program: Program,
                      currentLine: LineNumber): Gen[ProgramState] =
    for {
      stack <- genStack
      programState <- genProgramState(program, stack, currentLine)
    } yield programState

}
