package papercomputer

import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.numeric.{NonNegative, Positive}
import eu.timepit.refined.refineV
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.EitherValues.{
  convertLeftProjectionToValuable,
  convertRightProjectionToValuable
}
import org.scalatest.{Assertion, FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import cats.implicits._

class ProgramStateTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture {

    lazy val genProgramNextLineExists: Gen[(Program, LineNumber, LineNumber)] =
      for {
        program <- genNonEmptyProgram
          .suchThat(_.size >= 2) // todo replace with parameter like in RegistersTest
        allLines = program.keys
        anyTwoLines <- Gen.pick(2, allLines) // make sure there is 1 more line behind the lowest of 2 lines
        currentLine = anyTwoLines.minBy(_.value)
        (larger, _) = allLines.partition(_.value > currentLine.value)
        firstNextLine = larger.minBy(_.value)
      } yield (program, currentLine, firstNextLine)

    lazy val genProgramNoNextLine: Gen[(Program, LineNumber)] =
      genNonEmptyProgram
        .map(program => (program, program.keys.maxBy(_.value)))

    def genProgramState(program: Program,
                        currentLine: LineNumber): Gen[ProgramState] =
      for {
        stack <- genStack
        registers <- genRegisters
        programState = ProgramState(program,
                                    stack,
                                    Some(currentLine),
                                    registers).right.get
      } yield programState

    def genRefinedFromValueRange[P](minValue: Value, maxValue: Value)(
        implicit v: Validate[Value, P]): Gen[Refined[Value, P]] =
      // for performance reasons this assumes only correct values are refined within these tests,
      // so refineV will always be a Right
      Gen.chooseNum(minValue, maxValue).map(refineV[P](_).right.get)

    lazy val genLineNumber: Gen[LineNumber] =
      genRefinedFromValueRange[Positive](minLineNumber.value,
                                         maxLineNumber.value)

    lazy val genRegisterNumber: Gen[RegisterNumber] =
      genRefinedFromValueRange[NonNegative](minRegisterNumber.value,
                                            maxRegisterNumber.value)

    lazy val genCommand: Gen[Command] = {
      val genInc = genRegisterNumber.map(Inc)
      val genDec = genRegisterNumber.map(Dec)
      val genJmp = genLineNumber.map(Jmp)
      val genIsz = genRegisterNumber.map(Isz)
      val genStp = Gen.const(Stp)
      Gen.oneOf(genInc, genDec, genJmp, genIsz, genStp)
    }

    lazy val emptyProgram: Program = Map.empty

    def genProgramFromCommandlines(
        genSet: Gen[(LineNumber, Command)] => Gen[Set[(LineNumber, Command)]])
      : Gen[Program] =
      genSet(Gen.zip(genLineNumber, genCommand))
        .map(_.toMap)

    lazy val genProgram: Gen[Program] = genProgramFromCommandlines(
      Gen.containerOf[Set, (LineNumber, Command)])

    lazy val genNonEmptyProgram: Gen[Program] = genProgramFromCommandlines(
      Gen.nonEmptyContainerOf[Set, (LineNumber, Command)])

    lazy val genProgramAndAnyContainedLine: Gen[(Program, LineNumber)] =
      for {
        program <- genNonEmptyProgram
        anyLine <- Gen.oneOf(program.keys.toSeq)
      } yield (program, anyLine)

    def genProgramStateForIncDecOK(incDecF: RegisterNumber => IncDec)
    : Gen[(ProgramState, RegisterNumber, Registers, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        stack <- genStack
        ners <- genRegisters.suchThat(_.registerValues.nonEmpty) // todo
        registerNumber <- Gen.oneOf(ners.registerValues.keys.toSeq)
        program = nep.updated(currentLine, incDecF(registerNumber))
        programState = newProgramState(program, stack, Some(currentLine), ners)
      } yield (programState, registerNumber, ners, firstNextLine)

    def genProgramStateForIncDecNoNextLine(incDecF: RegisterNumber => IncDec): Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramNoNextLine
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, incDecF(registerNumber))
      programState <- genProgramState(program, currentLine)
    } yield programState

    def genProgramStateForFailingIncDec(incDecF: RegisterNumber => IncDec): Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, incDecF(registerNumber))
      programState <- genProgramState(program, currentLine)
    } yield programState




    lazy val genStack: Gen[Stack] = Gen.listOf(genProgramAndAnyContainedLine)

    lazy val genNonEmptyStack: Gen[Stack] =
      Gen.nonEmptyListOf(genProgramAndAnyContainedLine)

    lazy val genRegisterValue: Gen[RegisterValue] =
      Gen.chooseNum[RegisterValue](minRegisterValue, maxRegisterValue)

    lazy val genRegister: Gen[(RegisterNumber, RegisterValue)] =
      Gen.zip(genRegisterNumber, genRegisterValue)

    lazy val genRegisters: Gen[Registers] = for {
      registerValues <- Gen.mapOf(genRegister)
      // for performance reasons assume that during tests only valid registers are created
      // thus Registers() always returns a Right
      registers = Registers(minRegisterValue, maxRegisterValue, registerValues).right.get
    } yield registers

    implicit lazy val coGenRegisters: Cogen[Registers] = Cogen(
      (rs: Registers) => rs.hashCode.toLong)

    implicit lazy val coGenRegisterNumber: Cogen[RegisterNumber] = Cogen(
      (rn: RegisterNumber) => rn.hashCode.toLong)

    lazy val genMorRegisters: Gen[MorRegisters] = Gen.frequency(
      (1, Gen.const(Left(MessageDuringUnitTests))),
      (10, genRegisters.map(Right(_))))

    lazy val genRsToMorRs: Gen[Registers => MorRegisters] =
      Gen.function1[Registers, MorRegisters](genMorRegisters)

    lazy val genIncDecF: Gen[IncDecF] =
      Gen.function1[RegisterNumber, Registers => MorRegisters](genRsToMorRs)

    lazy val genMorBoolean: Gen[MorBoolean] = Gen.frequency(
      (1, Left(MessageDuringUnitTests)),
      (10, Arbitrary.arbBool.arbitrary.map(Right(_))))

    lazy val genRsToMorB: Gen[Registers => MorBoolean] =
      Gen.function1[Registers, MorBoolean](genMorBoolean)

    lazy val genIszF: Gen[IszF] =
      Gen.function1[RegisterNumber, Registers => MorBoolean](genRsToMorB)

    // create a new ProgramState under the assumption that all parameters are ok,
    // so it will always be a Right
    // todo check if this is needed?
    def newProgramState(program: Program,
                        stack: Stack,
                        currentLineO: Option[LineNumber],
                        registers: Registers): ProgramState =
      ProgramState(program, stack, currentLineO, registers).right.get

    def assertProgramState(programStateUnderTest: ProgramState,
                           program: Program,
                           stack: Stack,
                           currentLineO: Option[LineNumber],
                           registers: Registers): Assertion = {
      programStateUnderTest.program.shouldEqual(program)
      programStateUnderTest.stack.shouldEqual(stack)
      programStateUnderTest.currentLineO.shouldEqual(currentLineO)
      programStateUnderTest.registers.shouldEqual(registers)
    }

    def assertingIncDecF(expectedRn: RegisterNumber, expectedOldRs: Registers, resultingNewRs: Registers): IncDecF =
      (rn: RegisterNumber) => {
        rn.shouldEqual(expectedRn)
        rs: Registers =>
        {
          rs.shouldEqual(expectedOldRs)
          Right(resultingNewRs)
        }
      }

    def assertProgramState(origProgramState: ProgramState)(
        newProgramState: ProgramState,
        program: Program = origProgramState.program,
        stack: Stack = origProgramState.stack,
        currentLineO: Option[LineNumber] = origProgramState.currentLineO,
        registers: Registers = origProgramState.registers): Assertion =
      assertProgramState(newProgramState,
                         program,
                         stack,
                         currentLineO,
                         registers)

    def genProgramStateNextF(
        genIncF: Gen[IncF] = genIncDecF,
        genDecF: Gen[DecF] = genIncDecF,
        genIszF: Gen[IszF] = genIszF): Gen[ProgramState => MorProgramState] =
      for {
        incF <- genIncF
        decF <- genDecF
        iszF <- genIszF
      } yield ProgramState.next(incF, decF, iszF)

  }

  behavior of "ProgramState.apply(program, registers)"

  it should "return correct state for empty program" in new Fixture {
    forAll(genRegisters) { registers =>
      val morPs: MorProgramState = ProgramState(emptyProgram, registers)
      val ps: ProgramState = morPs.right.value
      // empty program shall be copied into the state
      // stack shall be empty
      // start line shall be None
      // registers shall be copied into the state
      assertProgramState(ps, emptyProgram, emptyStack, None, registers)
    }
  }

  it should "create correct state for non-empty program" in new Fixture {

    forAll(genNonEmptyProgram, genRegisters) { (program, registers) =>
      whenever(program.nonEmpty) {
        val morPs: MorProgramState = ProgramState(program, registers)
        val ps: ProgramState = morPs.right.value
        // program shall be copied into the state
        // stack shall be empty
        // start line should be lowest line in program
        // registers shall be copied into the state
        val expectedCurrentLineO = Some(program.keySet.minBy(_.value))
        assertProgramState(ps,
                           program,
                           emptyStack,
                           expectedCurrentLineO,
                           registers)
      }
    }
  }

  behavior of "ProgramState.apply(program, stack, startLineO, registers)"

  it should "create correct state for empty startLine and empty stack" in new Fixture {
    forAll(genProgram, genRegisters) { (program, registers) =>
      val morPs: MorProgramState =
        ProgramState(program, emptyStack, None, registers)
      val ps: ProgramState = morPs.right.value
      // program shall be copied into the state
      // stack shall be empty
      // start line shall be None
      // registers shall be copied into the state
      assertProgramState(ps, program, emptyStack, None, registers)
    }
  }

  it should "fail for empty startLine and non-empty stack" in new Fixture {
    forAll(genNonEmptyProgram, genNonEmptyStack, genRegisters) {
      (program, nonEmptyStack, registers) =>
        val morPs: MorProgramState =
          ProgramState(program, nonEmptyStack, None, registers)
        morPs.left.value.shouldEqual(IllegalState)
    }
  }

  it should "fail for non-empty startLine not in program" in new Fixture {
    forAll(genProgram, genLineNumber, genRegisters) {
      (program, startLine: LineNumber, registers) =>
        whenever(!program.contains(startLine)) {
          val morPs: MorProgramState =
            ProgramState(program, emptyStack, Some(startLine), registers)
          morPs.left.value.shouldEqual(StartLineNotFoundInProgram)
        }
    }
  }

  it should "fail for correct non-empty start line but an invalid stack" in new Fixture {
    def gen: Gen[(Program, LineNumber, Stack)] =
      for {
        (nep, startLine) <- genProgramAndAnyContainedLine
        nonContainedLine <- genLineNumber.suchThat(!nep.contains(_))
        partStack <- genStack
        stack = partStack ++ List((nep, nonContainedLine)) ++ partStack
      } yield (nep, startLine, stack)

    forAll(gen, genRegisters) {
      case ((program, startLine: LineNumber, stack), registers: Registers) =>
        val morPs: MorProgramState =
          ProgramState(program, stack, Some(startLine), registers)
        morPs.left.value.shouldEqual(IllegalReferenceToNonExistingLineNumber)
    }
  }

  it should "create correct state for correct non-empty start line and correct stack" in new Fixture {
    forAll(genProgramAndAnyContainedLine, genStack, genRegisters) {
      case ((program, startLine: LineNumber), stack, registers) =>
        whenever(program.nonEmpty) {
          val morPs: MorProgramState =
            ProgramState(program, stack, Some(startLine), registers)
          val ps: ProgramState = morPs.right.value
          // program shall be copied into the state
          // stack shall be copied into the state
          // start line should be given line
          // registers shall be copied into the state
          assertProgramState(ps, program, stack, Some(startLine), registers)
        }
    }
  }

  behavior of "ProgramState.next if currentLine is None, which indicates end of program"

  it should "fail with CannotRunAFinishedProgram" in new Fixture {
    def genPs: Gen[ProgramState] =
      for {
        program <- genProgram
        rs <- genRegisters
      } yield newProgramState(program, emptyStack, None, rs)

    forAll(genIncDecF, genIncDecF, genIszF, genPs) {
      (incF, decF, iszF, programState) =>
        val morPs: MorProgramState =
          ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(CannotRunAFinishedProgram)
    }
  }

  behavior of "ProgramState.next for Inc"

  it should "return correct state if incF succeeds and next line exists" in new Fixture {
    forAll(genIncDecF, genIszF, genProgramStateForIncDecOK(Inc), genRegisters) {
      case (decF,
            iszF,
            (programState, registerNumber, oldRegisters, firstNextLine),
            newRegisters) =>
        val incF = assertingIncDecF(registerNumber, oldRegisters, newRegisters)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be firstNextLine
        // registers shall be updated
        assertProgramState(programState)(newProgramState,
                                         currentLineO = Some(firstNextLine),
                                         registers = newRegisters)
    }
  }

  it should "fail if incF succeeds but next line does not exist" in new Fixture {
    forAll(genIncDecF, genIszF, genProgramStateForIncDecNoNextLine(Inc), genRegisters) {
      (decF, iszF, programState, newRegisters) =>
        val incF: IncF = _ => _ => Right(newRegisters)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if provided incF fails" in new Fixture {
    forAll(genIncDecF, genIszF, genProgramStateForFailingIncDec(Inc)) {
      (decF, iszF, programState) =>
        val incF: IncF = _ => _ => Left(MessageDuringUnitTests)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(MessageDuringUnitTests)
    }
  }

  // todo remove more parallelism between Inc and Dec
  behavior of "ProgramState.next for Dec"

  it should "return correct state if decF succeeds and next line exists" in new Fixture {
    forAll(genIncDecF, genIszF, genProgramStateForIncDecOK(Dec), genRegisters) {
      case (incF,
            iszF,
            (programState, registerNumber, oldRegisters, firstNextLine),
            newRegisters) =>
        val decF = assertingIncDecF(registerNumber, oldRegisters, newRegisters)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be firstNextLine
        // registers shall be updated
        assertProgramState(programState)(newProgramState,
                                         currentLineO = Some(firstNextLine),
                                         registers = newRegisters)
    }
  }

  it should "fail if decF succeeds but next line does not exist" in new Fixture {
    forAll(genIncDecF, genIszF, genProgramStateForIncDecNoNextLine(Dec), genRegisters) {
      (incF, iszF, programState, newRegisters) =>
        val decF: DecF = _ => _ => Right(newRegisters)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if provided decF fails" in new Fixture {
    forAll(genIncDecF, genIszF, genProgramStateForFailingIncDec(Dec)) {
      (incF, iszF, programState) =>
        val decF: DecF = _ => _ => Left(MessageDuringUnitTests)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(MessageDuringUnitTests)
    }
  }

  behavior of "ProgramState.next for Jmp"

  it should "return correct state if line exists" in new Fixture {
    val genProgramStateForJmpOK: Gen[(ProgramState, LineNumber)] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      jmpLine <- Gen.oneOf(nep.keys.toSeq)
      program = nep.updated(currentLine, Jmp(jmpLine))
      programState <- genProgramState(program, currentLine)
    } yield (programState, jmpLine)

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForJmpOK) {
      case (incF, decF, iszF, (programState, jmpLine)) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be jmpLine
        assertProgramState(programState)(newProgramState,
                                         currentLineO = Some(jmpLine))
    }
  }

  it should "fail if line does not exist" in new Fixture {
    val genProgramStateForJmpNOK: Gen[ProgramState] =
      for {
        (nep, currentLine) <- genProgramAndAnyContainedLine
        jmpLine <- genLineNumber.suchThat(!nep.contains(_))
        program = nep.updated(currentLine, Jmp(jmpLine))
        programState <- genProgramState(program, currentLine)
      } yield programState

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForJmpNOK) {
      (incF, decF, iszF, programState) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(IllegalReferenceToNonExistingLineNumber)
    }
  }

  behavior of "ProgramState.next for Isz"

  it should "return correct state if iszF returns true and second next line exists" in new Fixture {
    val genProgramStateForIszTrueOK: Gen[(ProgramState, LineNumber)] = for {
      nep <- genNonEmptyProgram.suchThat(_.size >= 3) // todo
      allLines = nep.keys
      anyThreeLines <- Gen.pick(3, allLines) // make sure there are 2 more lines behind the lowest of 3 lines
      startLine = anyThreeLines.minBy(_.value)
      (larger, _) = allLines.partition(_.value > startLine.value)
      firstNextLine = larger.minBy(_.value)
      secondNextLine = larger.filterNot(_ == firstNextLine).minBy(_.value)
      stack <- genStack
      ners <- genRegisters.suchThat(_.registerValues.nonEmpty) // todo
      registerNumber <- genRegisterNumber
      program = nep.updated(startLine, Isz(registerNumber))
      programState = ProgramState(program, stack, Some(startLine), ners).right.get
    } yield (programState, secondNextLine)

    forAll(genIncDecF, genIncDecF, genProgramStateForIszTrueOK) {
      case (incF, decF, (programState, secondNextLine)) =>
        val iszF: IszF = _ => _ => Right(true)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be secondNextLine
        assertProgramState(programState)(newProgramState,
                                         currentLineO = Some(secondNextLine))
    }
  }

  it should "fail if iszF returns true and second next line does not exist" in new Fixture {
    val genProgramStateForIszTrueNOK: Gen[ProgramState] = for {
      nep <- genNonEmptyProgram.suchThat(_.size >= 2)
      allLines = nep.keys
      lastLine = allLines.maxBy(_.value)
      currentLine = allLines.filterNot(_ == lastLine).maxBy(_.value)
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genIncDecF, genIncDecF, genProgramStateForIszTrueNOK) {
      (incF, decF, programState) =>
        val iszF: IszF = _ => _ => Right(true)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "return correct state if iszF returns false and next line exists" in new Fixture {
    val genProgramStateForIszFalseOK: Gen[(ProgramState, LineNumber)] = for {
      (nep, currentLine, firstNextLine) <- genProgramNextLineExists
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
      programState <- genProgramState(program, currentLine)
    } yield (programState, firstNextLine)

    forAll(genIncDecF, genIncDecF, genProgramStateForIszFalseOK) {
      case (incF, decF, (programState, firstNextLine)) =>
        val iszF: IszF = _ => _ => Right(false)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be firstNextLine
        assertProgramState(programState)(newProgramState,
                                         currentLineO = Some(firstNextLine))
    }
  }

  it should "fail if iszF returns false and next line does not exist" in new Fixture {
    val genProgramStateForIszFalseNOK: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramNoNextLine
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genIncDecF, genIncDecF, genProgramStateForIszFalseNOK) {
      (incF, decF, programState) =>
        val iszF: IszF = _ => _ => Right(true)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if iszF fails" in new Fixture {
    val genProgramStateForIszFails: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genIncDecF, genIncDecF, genProgramStateForIszFails) {
      (incF, decF, programState) =>
        val iszF: IszF = _ => _ => Left(MessageDuringUnitTests)
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(MessageDuringUnitTests)
    }
  }

  behavior of "ProgramState.next for Stp"

  it should "return correct state on empty stack" in new Fixture {
    val genProgramStateForStp: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      program = nep.updated(currentLine, Stp)
      registers <- genRegisters
    } yield newProgramState(program, emptyStack, Some(currentLine), registers)

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForStp) {
      (incF, decF, iszF, programState) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be None
        assertProgramState(programState)(newProgramState, currentLineO = None)
    }
  }

  it should "return correct state on non-empty stack" in new Fixture {
    val genProgramStateForStp: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      program = nep.updated(currentLine, Stp)
      stack <- genNonEmptyStack
      registers <- genRegisters
    } yield newProgramState(program, stack, Some(currentLine), registers)

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForStp) {
      (incF, decF, iszF, programState) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // stack shall be the tail of the original stack
        // current program and current line shall come from the head of the original stack
        val (newProgram, newCurrentLine) :: newStack = programState.stack
        assertProgramState(programState)(newProgramState,
                                         program = newProgram,
                                         stack = newStack,
                                         currentLineO = Some(newCurrentLine))
    }
  }

  behavior of "ProgramState.next for Sub"

  it should "return correct state if sub line and next line exist" in new Fixture {
    val genProgramStateForSubOK: Gen[(ProgramState, LineNumber, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        subLine <- Gen.oneOf(nep.keys.toSeq)
        program = nep.updated(currentLine, Sub(subLine))
        programState <- genProgramState(program, currentLine)
      } yield (programState, subLine, firstNextLine)

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForSubOK) {
      case (incF, decF, iszF, (programState, subLine, firstNextLine)) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // new current line shall be subLine
        // new stack stall be old stack with (currentProgram, firstNextLine) prepended
        assertProgramState(programState)(
          newProgramState,
          stack = (programState.program, firstNextLine) +: programState.stack,
          currentLineO = Some(subLine))
    }
  }

  it should "fail if sub line exists but next line does not exist" in new Fixture {
    val genProgramStateForSubNoNextLine: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramNoNextLine
      subLine <- Gen.oneOf(nep.keys.toSeq)
      program = nep.updated(currentLine, Sub(subLine))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForSubNoNextLine) {
      (incF, decF, iszF, programState) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if next line exists but sub line does not exist" in new Fixture {
    val genProgramStateForSubNoNextLine: Gen[ProgramState] = for {
      (nep, currentLine, _) <- genProgramNextLineExists
      subLine <- genLineNumber.suchThat(!nep.keys.toSeq.contains(_))
      program = nep.updated(currentLine, Sub(subLine))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForSubNoNextLine) {
      (incF, decF, iszF, programState) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(IllegalReferenceToNonExistingLineNumber)
    }
  }

  behavior of "ProgramState.next for Prg"

  it should "return correct state for non-empty sub programs and next line exists" in new Fixture {
    val genProgramStateForNonEmptyPrg
      : Gen[(ProgramState, Program, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        subPrg <- genNonEmptyProgram
        program = nep.updated(currentLine, Prg(subPrg))
        programState <- genProgramState(program, currentLine)
      } yield (programState, subPrg, firstNextLine)

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForNonEmptyPrg) {
      case (incF, decF, iszF, (programState, subProgram, firstNextLine)) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // new program shall be subProgram
        // new stack stall be old stack with old program and firstNextLine prepended
        val newStack
          : Stack = (programState.program, firstNextLine) +: programState.stack
        assertProgramState(programState)(
          newProgramState,
          program = subProgram,
          stack = newStack,
          currentLineO = Some(subProgram.keys.minBy(_.value))
        )
    }
  }

  it should "skip empty sub program if next line exists" in new Fixture {
    val genProgramStateForEmptyPrg: Gen[(ProgramState, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        subPrg = emptyProgram
        program = nep.updated(currentLine, Prg(subPrg))
        programState <- genProgramState(program, currentLine)
      } yield (programState, firstNextLine)

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForEmptyPrg) {
      case (incF, decF, iszF, (programState, firstNextLine)) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        val newProgramState: ProgramState = morPs.right.value
        // new program shall be subProgram
        // new stack stall be old stack with old program and firstNextLine prepended
        assertProgramState(programState)(newProgramState,
                                         currentLineO = Some(firstNextLine))
    }
  }

  it should "fail if next line does not exist" in new Fixture {
    val genProgramStateForPrgNoNextLine: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramNoNextLine
      subPrg <- genProgram
      program = nep.updated(currentLine, Prg(subPrg))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genIncDecF, genIncDecF, genIszF, genProgramStateForPrgNoNextLine) {
      (incF, decF, iszF, programState) =>
        val morPs = ProgramState.next(incF, decF, iszF)(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

}
