package papercomputer

import eu.timepit.refined.auto.autoRefineV
import fs2.{Pure, Stream}
import org.scalacheck.Gen
import org.scalatest.EitherValues.{
  convertLeftProjectionToValuable,
  convertRightProjectionToValuable
}
import org.scalatest.{Assertion, FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProgramStateTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture extends CommonFixtures {
    lazy val genProgram: Gen[Program] = genProgramFromCommandlines(
      Gen.containerOf[Set, (LineNumber, Command)])

    lazy val genProgramNextLineExists: Gen[(Program, LineNumber, LineNumber)] =
      for {
        program <- genNonEmptyProgramMinLines(2)
        allLines = program.keys
        anyTwoLines <- Gen.pick(2, allLines) // make sure there is 1 more line behind the lowest of 2 lines
        currentLine = anyTwoLines.minBy(_.value)
        (larger, _) = allLines.partition(_.value > currentLine.value)
        firstNextLine = larger.minBy(_.value)
      } yield (program, currentLine, firstNextLine)

    lazy val genProgramNoNextLine: Gen[(Program, LineNumber)] =
      genNonEmptyProgram
        .map(program => (program, program.keys.maxBy(_.value)))

    def genProgramForIncDecOK(incDecF: RegisterNumber => IncDec)
      : Gen[(Program, LineNumber, RegisterNumber, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        registerNumber <- genRegisterNumber
        program = nep.updated(currentLine, incDecF(registerNumber))
      } yield (program, currentLine, registerNumber, firstNextLine)

    def genProgramStateForIncDecNoNextLine(
        incDecF: RegisterNumber => IncDec): Gen[(Program, LineNumber)] =
      for {
        (nep, currentLine) <- genProgramNoNextLine
        registerNumber <- genRegisterNumber
        program = nep.updated(currentLine, incDecF(registerNumber))
      } yield (program, currentLine)

    def genProgramStateForFailingIncDec(
        incDecF: RegisterNumber => IncDec): Gen[(Program, LineNumber)] =
      for {
        (nep, currentLine) <- genProgramAndAnyContainedLine
        registerNumber <- genRegisterNumber
        program = nep.updated(currentLine, incDecF(registerNumber))
      } yield (program, currentLine)

    lazy val genNonEmptyStack: Gen[Stack] =
      Gen.nonEmptyListOf(genProgramAndAnyContainedLine)

    def assertingIncDecF(expectedRn: RegisterNumber,
                         expectedOldRs: Registers,
                         resultingNewRs: Registers): IncDecF =
      (rn: RegisterNumber) => {
        rn.shouldEqual(expectedRn)
        rs: Registers =>
          {
            rs.shouldEqual(expectedOldRs)
            Right(resultingNewRs)
          }
      }

    def assertingIszF(expectedRn: RegisterNumber,
                      expectedRs: Registers,
                      resultingIsZero: Boolean): IszF =
      (rn: RegisterNumber) => {
        rn.shouldEqual(expectedRn)
        rs: Registers =>
          {
            rs.shouldEqual(expectedRs)
            Right(resultingIsZero)
          }
      }

    def assertProgramState(oldProgramState: ProgramState)(
        programStateUnderTest: ProgramState,
        newRegistersOpsConfig: RegistersOpsConfig =
          oldProgramState.registersOpsConfig,
        newProgram: Program = oldProgramState.program,
        newStack: Stack = oldProgramState.stack,
        newCurrentLineO: Option[LineNumber] = oldProgramState.currentLineO,
        newRegisters: Registers = oldProgramState.registers): Assertion =
      assertProgramState(programStateUnderTest,
                         newRegistersOpsConfig,
                         newProgram,
                         newStack,
                         newCurrentLineO,
                         newRegisters)

    def assertProgramState(programStateUnderTest: ProgramState,
                           registersOpsConfig: RegistersOpsConfig,
                           program: Program,
                           stack: Stack,
                           currentLineO: Option[LineNumber],
                           registers: Registers): Assertion = {
      programStateUnderTest.registersOpsConfig.shouldEqual(registersOpsConfig)
      programStateUnderTest.program.shouldEqual(program)
      programStateUnderTest.stack.shouldEqual(stack)
      programStateUnderTest.currentLineO.shouldEqual(currentLineO)
      programStateUnderTest.registers.shouldEqual(registers)
    }
  }

  behavior of "ProgramState.morPsFromProgramStateConfig"

  it should "return correct state for empty program" in new Fixture {
    forAll(genRegisters) { registers =>
      val registersConfig = RegistersConfig(registers.minRegisterValue,
                                            registers.maxRegisterValue,
                                            registers.registerValues)
      val programStateConfig = ProgramStateConfig(emptyProgram, registersConfig)
      val morPs: Mor[ProgramState] =
        ProgramState.morPsFromProgramStateConfig(programStateConfig)
      val ps: ProgramState = morPs.right.value
      // config shall come from Registers functions
      // empty program shall be copied into the state
      // stack shall be empty
      // start line shall be None
      // registers shall be copied into the state
      val expectedRegistersOpsConfig =
        RegistersOpsConfig(RegistersOps.inc, RegistersOps.dec, RegistersOps.isz)
      assertProgramState(ps,
                         expectedRegistersOpsConfig,
                         emptyProgram,
                         emptyStack,
                         None,
                         registers)
    }
  }

  it should "create correct state for non-empty program" in new Fixture {
    forAll(genNonEmptyProgram, genRegisters) { (program, registers) =>
      val registersConfig = RegistersConfig(registers.minRegisterValue,
                                            registers.maxRegisterValue,
                                            registers.registerValues)
      val programStateConfig = ProgramStateConfig(program, registersConfig)
      val morPs: Mor[ProgramState] =
        ProgramState.morPsFromProgramStateConfig(programStateConfig)
      val ps: ProgramState = morPs.right.value
      // config shall come from Registers functions
      // program shall be copied into the state
      // stack shall be empty
      // start line should be lowest line in program
      // registers shall be copied into the state
      val expectedRegistersOpsConfig =
        RegistersOpsConfig(RegistersOps.inc, RegistersOps.dec, RegistersOps.isz)
      val expectedCurrentLineO = Some(program.keySet.minBy(_.value))
      assertProgramState(ps,
                         expectedRegistersOpsConfig,
                         program,
                         emptyStack,
                         expectedCurrentLineO,
                         registers)
    }
  }

  behavior of "ProgramState.apply(config, program, stack, startLineO, registers)"

  it should "create correct state for empty startLine and empty stack" in new Fixture {
    forAll(genRegistersOpsConfig, genProgram, genRegisters) {
      (registersOpsConfig, program, registers) =>
        val morPs: Mor[ProgramState] =
          ProgramState(registersOpsConfig, program, emptyStack, None, registers)
        val ps: ProgramState = morPs.right.value
        // program shall be copied into the state
        // stack shall be empty
        // start line shall be None
        // registers shall be copied into the state
        assertProgramState(ps,
                           registersOpsConfig,
                           program,
                           emptyStack,
                           None,
                           registers)
    }
  }

  it should "fail for empty startLine and non-empty stack" in new Fixture {
    forAll(genRegistersOpsConfig,
           genNonEmptyProgram,
           genNonEmptyStack,
           genRegisters) {
      (registersOpsConfig, program, nonEmptyStack, registers) =>
        val morPs: Mor[ProgramState] =
          ProgramState(registersOpsConfig,
                       program,
                       nonEmptyStack,
                       None,
                       registers)
        morPs.left.value.shouldEqual(IllegalState)
    }
  }

  it should "fail for non-empty startLine not in program" in new Fixture {
    forAll(genRegistersOpsConfig, genProgram, genLineNumber, genRegisters) {
      (registersOpsConfig, program, startLine: LineNumber, registers) =>
        whenever(!program.contains(startLine)) {
          val morPs: Mor[ProgramState] =
            ProgramState(registersOpsConfig,
                         program,
                         emptyStack,
                         Some(startLine),
                         registers)
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

    forAll(genRegistersOpsConfig, gen, genRegisters) {
      case (registersOpsConfig,
            (program, startLine: LineNumber, stack),
            registers: Registers) =>
        val morPs: Mor[ProgramState] =
          ProgramState(registersOpsConfig,
                       program,
                       stack,
                       Some(startLine),
                       registers)
        morPs.left.value.shouldEqual(IllegalReferenceToNonExistingLineNumber)
    }
  }

  it should "create correct state for correct non-empty start line and correct stack" in new Fixture {
    forAll(genRegistersOpsConfig,
           genProgramAndAnyContainedLine,
           genStack,
           genRegisters) {
      case (registersOpsConfig,
            (program, startLine: LineNumber),
            stack,
            registers) =>
        whenever(program.nonEmpty) {
          val morPs: Mor[ProgramState] =
            ProgramState(registersOpsConfig,
                         program,
                         stack,
                         Some(startLine),
                         registers)
          val ps: ProgramState = morPs.right.value
          // program shall be copied into the state
          // stack shall be copied into the state
          // start line should be given line
          // registers shall be copied into the state
          assertProgramState(ps,
                             registersOpsConfig,
                             program,
                             stack,
                             Some(startLine),
                             registers)
        }
    }
  }

  behavior of "ProgramState.nextPs if currentLine is None, which indicates end of program"

  it should "fail with CannotRunAFinishedProgram" in new Fixture {
    def genPs: Gen[ProgramState] =
      for {
        registersOpsConfig <- genRegistersOpsConfig
        program <- genProgram
        rs <- genRegisters
        programState = newProgramState(registersOpsConfig,
                                       program,
                                       emptyStack,
                                       None,
                                       rs)
      } yield programState

    forAll(genPs) { programState =>
      val morPs: Mor[ProgramState] =
        ProgramState.nextPs(programState)
      morPs.left.value.shouldEqual(CannotRunAFinishedProgram)
    }
  }

  behavior of "ProgramState.nextPs for Inc"

  it should "return correct state if incF succeeds and next line exists" in new Fixture {
    forAll(genIncDecF,
           genIszF,
           genProgramForIncDecOK(Inc),
           genStack,
           genRegisters,
           genRegisters) {
      case (decF,
            iszF,
            (program, currentLine, registerNumber, firstNextLine),
            stack,
            oldRegisters,
            newRegisters) =>
        val incF: IncDecF =
          assertingIncDecF(registerNumber, oldRegisters, newRegisters)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState = newProgramState(config,
                                           program,
                                           stack,
                                           Some(currentLine),
                                           oldRegisters)
        val morPs = ProgramState.nextPs(programState)
        val nextProgramState: ProgramState = morPs.right.value
        // current line shall be firstNextLine
        // registers shall be updated
        assertProgramState(programState)(nextProgramState,
                                         newCurrentLineO = Some(firstNextLine),
                                         newRegisters = newRegisters)
    }
  }

  it should "fail if incF succeeds but next line does not exist" in new Fixture {
    forAll(genIncDecF,
           genIszF,
           genProgramStateForIncDecNoNextLine(Inc),
           genStack,
           genRegisters,
           genRegisters) {
      case (decF,
            iszF,
            (program, currentLine),
            stack,
            oldRegisters,
            newRegisters) =>
        val incF: IncDecF = _ => _ => Right(newRegisters)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState = newProgramState(config,
                                           program,
                                           stack,
                                           Some(currentLine),
                                           oldRegisters)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if provided incF fails" in new Fixture {
    forAll(genIncDecF,
           genIszF,
           genProgramStateForFailingIncDec(Inc),
           genStack,
           genRegisters) {
      case (decF, iszF, (program, currentLine), stack, oldRegisters) =>
        val incF: IncDecF = _ => _ => Left(MessageDuringUnitTests)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState = newProgramState(config,
                                           program,
                                           stack,
                                           Some(currentLine),
                                           oldRegisters)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(MessageDuringUnitTests)
    }
  }

  // todo remove more parallelism between Inc and Dec
  behavior of "ProgramState.nextPs for Dec"

  it should "return correct state if decF succeeds and next line exists" in new Fixture {
    forAll(genIncDecF,
           genIszF,
           genProgramForIncDecOK(Dec),
           genStack,
           genRegisters,
           genRegisters) {
      case (incF,
            iszF,
            (program, currentLine, registerNumber, firstNextLine),
            stack,
            oldRegisters,
            newRegisters) =>
        val decF: IncDecF =
          assertingIncDecF(registerNumber, oldRegisters, newRegisters)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState = newProgramState(config,
                                           program,
                                           stack,
                                           Some(currentLine),
                                           oldRegisters)
        val morPs = ProgramState.nextPs(programState)
        val nextProgramState: ProgramState = morPs.right.value
        // current line shall be firstNextLine
        // registers shall be updated
        assertProgramState(programState)(nextProgramState,
                                         newCurrentLineO = Some(firstNextLine),
                                         newRegisters = newRegisters)
    }
  }

  it should "fail if decF succeeds but next line does not exist" in new Fixture {
    forAll(genIncDecF,
           genIszF,
           genProgramStateForIncDecNoNextLine(Dec),
           genStack,
           genRegisters,
           genRegisters) {
      case (incF,
            iszF,
            (program, currentLine),
            stack,
            oldRegisters,
            newRegisters) =>
        val decF: IncDecF = _ => _ => Right(newRegisters)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState = newProgramState(config,
                                           program,
                                           stack,
                                           Some(currentLine),
                                           oldRegisters)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if provided decF fails" in new Fixture {
    forAll(genIncDecF,
           genIszF,
           genProgramStateForFailingIncDec(Dec),
           genStack,
           genRegisters) {
      case (incF, iszF, (program, currentLine), stack, oldRegisters) =>
        val decF: IncDecF = _ => _ => Left(MessageDuringUnitTests)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState = newProgramState(config,
                                           program,
                                           stack,
                                           Some(currentLine),
                                           oldRegisters)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(MessageDuringUnitTests)
    }
  }

  behavior of "ProgramState.nextPs for Jmp"

  it should "return correct state if line exists" in new Fixture {
    val genProgramStateForJmpOK: Gen[(ProgramState, LineNumber)] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      jmpLine <- Gen.oneOf(nep.keys.toSeq)
      program = nep.updated(currentLine, Jmp(jmpLine))
      programState <- genProgramState(program, currentLine)
    } yield (programState, jmpLine)

    forAll(genProgramStateForJmpOK) {
      case (programState, jmpLine) =>
        val morPs = ProgramState.nextPs(programState)
        val newProgramState: ProgramState = morPs.right.value
        // current line shall be jmpLine
        assertProgramState(programState)(newProgramState,
                                         newCurrentLineO = Some(jmpLine))
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

    forAll(genProgramStateForJmpNOK) { programState =>
      val morPs = ProgramState.nextPs(programState)
      morPs.left.value.shouldEqual(IllegalReferenceToNonExistingLineNumber)
    }
  }

  behavior of "ProgramState.nextPs for Isz"

  it should "return correct state if iszF returns true and second next line exists" in new Fixture {
    val genProgramStateForIszTrueOK
      : Gen[(Program, LineNumber, RegisterNumber, LineNumber)] = for {
      nep <- genNonEmptyProgramMinLines(3)
      allLines = nep.keys
      anyThreeLines <- Gen.pick(3, allLines) // make sure there are 2 more lines behind the lowest of 3 lines
      currentLine = anyThreeLines.minBy(_.value)
      (larger, _) = allLines.partition(_.value > currentLine.value)
      firstNextLine = larger.minBy(_.value)
      secondNextLine = larger.filterNot(_ == firstNextLine).minBy(_.value)
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
    } yield (program, currentLine, registerNumber, secondNextLine)

    forAll(genIncDecF,
           genIncDecF,
           genProgramStateForIszTrueOK,
           genStack,
           genRegisters) {
      case (incF,
            decF,
            (program, currentLine, registerNumber, secondNextLine),
            stack,
            registers) =>
        val iszF: IszF =
          assertingIszF(registerNumber, registers, resultingIsZero = true)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState =
          newProgramState(config, program, stack, Some(currentLine), registers)
        val morPs = ProgramState.nextPs(programState)
        val nextProgramState: ProgramState = morPs.right.value
        // current line shall be secondNextLine
        assertProgramState(programState)(nextProgramState,
                                         newCurrentLineO = Some(secondNextLine))
    }
  }

  it should "fail if iszF returns true and second next line does not exist" in new Fixture {
    val genProgramStateForIszTrueNOK: Gen[(Program, LineNumber)] = for {
      nep <- genNonEmptyProgramMinLines(2)
      allLines = nep.keys
      lastLine = allLines.maxBy(_.value)
      currentLine = allLines.filterNot(_ == lastLine).maxBy(_.value)
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
    } yield (program, currentLine)

    forAll(genIncDecF,
           genIncDecF,
           genProgramStateForIszTrueNOK,
           genStack,
           genRegisters) {
      case (incF, decF, (program, currentLine), stack, registers) =>
        val iszF: IszF = _ => _ => Right(true)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState =
          newProgramState(config, program, stack, Some(currentLine), registers)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "return correct state if iszF returns false and next line exists" in new Fixture {
    val genProgramStateForIszFalseOK: Gen[(Program, LineNumber, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        registerNumber <- genRegisterNumber
        program = nep.updated(currentLine, Isz(registerNumber))
      } yield (program, currentLine, firstNextLine)

    forAll(genIncDecF,
           genIncDecF,
           genProgramStateForIszFalseOK,
           genStack,
           genRegisters) {
      case (incF,
            decF,
            (program, currentLine, firstNextLine),
            stack,
            registers) =>
        val iszF: IszF = _ => _ => Right(false)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState =
          newProgramState(config, program, stack, Some(currentLine), registers)
        val morPs = ProgramState.nextPs(programState)
        val nextProgramState: ProgramState = morPs.right.value
        // current line shall be firstNextLine
        assertProgramState(programState)(nextProgramState,
                                         newCurrentLineO = Some(firstNextLine))
    }
  }

  it should "fail if iszF returns false and next line does not exist" in new Fixture {
    val genProgramStateForIszFalseNOK: Gen[(Program, LineNumber)] = for {
      (nep, currentLine) <- genProgramNoNextLine
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
    } yield (program, currentLine)

    forAll(genIncDecF,
           genIncDecF,
           genProgramStateForIszFalseNOK,
           genStack,
           genRegisters) {
      case (incF, decF, (program, currentLine), stack, registers) =>
        val iszF: IszF = _ => _ => Right(false)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState =
          newProgramState(config, program, stack, Some(currentLine), registers)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  it should "fail if iszF fails" in new Fixture {
    val genProgramStateForIszFails: Gen[(Program, LineNumber)] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      registerNumber <- genRegisterNumber
      program = nep.updated(currentLine, Isz(registerNumber))
    } yield (program, currentLine)

    forAll(genIncDecF,
           genIncDecF,
           genProgramStateForIszFails,
           genStack,
           genRegisters) {
      case (incF, decF, (program, currentLine), stack, registers) =>
        val iszF: IszF = _ => _ => Left(MessageDuringUnitTests)
        val config = RegistersOpsConfig(incF, decF, iszF)
        val programState =
          newProgramState(config, program, stack, Some(currentLine), registers)
        val morPs = ProgramState.nextPs(programState)
        morPs.left.value.shouldEqual(MessageDuringUnitTests)
    }
  }

  behavior of "ProgramState.nextPs for Stp"

  it should "return correct state on empty stack" in new Fixture {
    val genProgramStateForStp: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      program = nep.updated(currentLine, Stp)
      programState <- genProgramState(program, emptyStack, currentLine)
    } yield programState

    forAll(genProgramStateForStp) { programState =>
      val morPs = ProgramState.nextPs(programState)
      val newProgramState: ProgramState = morPs.right.value
      // current line shall be None
      assertProgramState(programState)(newProgramState, newCurrentLineO = None)
    }
  }

  it should "return correct state on non-empty stack" in new Fixture {
    val genProgramStateForStp: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramAndAnyContainedLine
      program = nep.updated(currentLine, Stp)
      stack <- genNonEmptyStack
      programState <- genProgramState(program, stack, currentLine)
    } yield programState

    forAll(genProgramStateForStp) { programState =>
      val morPs = ProgramState.nextPs(programState)
      val newProgramState: ProgramState = morPs.right.value
      // stack shall be the tail of the original stack
      // current program and current line shall come from the head of the original stack
      val (newProgram, newCurrentLine) :: newStack = programState.stack
      assertProgramState(programState)(newProgramState,
                                       newProgram = newProgram,
                                       newStack = newStack,
                                       newCurrentLineO = Some(newCurrentLine))
    }
  }

  behavior of "ProgramState.nextPs for Sub"

  it should "return correct state if sub line and next line exist" in new Fixture {
    val genProgramStateForSubOK: Gen[(ProgramState, LineNumber, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        subLine <- Gen.oneOf(nep.keys.toSeq)
        program = nep.updated(currentLine, Sub(subLine))
        programState <- genProgramState(program, currentLine)
      } yield (programState, subLine, firstNextLine)

    forAll(genProgramStateForSubOK) {
      case (programState, subLine, firstNextLine) =>
        val morPs = ProgramState.nextPs(programState)
        val newProgramState: ProgramState = morPs.right.value
        // new current line shall be subLine
        // new stack stall be old stack with (currentProgram, firstNextLine) prepended
        assertProgramState(programState)(
          newProgramState,
          newStack = (programState.program, firstNextLine) +: programState.stack,
          newCurrentLineO = Some(subLine))
    }
  }

  it should "fail if sub line exists but next line does not exist" in new Fixture {
    val genProgramStateForSubNoNextLine: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramNoNextLine
      subLine <- Gen.oneOf(nep.keys.toSeq)
      program = nep.updated(currentLine, Sub(subLine))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genProgramStateForSubNoNextLine) { programState =>
      val morPs = ProgramState.nextPs(programState)
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

    forAll(genProgramStateForSubNoNextLine) { programState =>
      val morPs = ProgramState.nextPs(programState)
      morPs.left.value.shouldEqual(IllegalReferenceToNonExistingLineNumber)
    }
  }

  behavior of "ProgramState.nextPs for Prg"

  it should "return correct state for non-empty sub programs and next line exists" in new Fixture {
    val genProgramStateForNonEmptyPrg
      : Gen[(ProgramState, Program, LineNumber)] =
      for {
        (nep, currentLine, firstNextLine) <- genProgramNextLineExists
        subPrg <- genNonEmptyProgram
        program = nep.updated(currentLine, Prg(subPrg))
        programState <- genProgramState(program, currentLine)
      } yield (programState, subPrg, firstNextLine)

    forAll(genProgramStateForNonEmptyPrg) {
      case (programState, subProgram, firstNextLine) =>
        val morPs = ProgramState.nextPs(programState)
        val newProgramState: ProgramState = morPs.right.value
        // new program shall be subProgram
        // new stack stall be old stack with old program and firstNextLine prepended
        val newStack
          : Stack = (programState.program, firstNextLine) +: programState.stack
        assertProgramState(programState)(
          newProgramState,
          newProgram = subProgram,
          newStack = newStack,
          newCurrentLineO = Some(subProgram.keys.minBy(_.value))
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

    forAll(genProgramStateForEmptyPrg) {
      case (programState, firstNextLine) =>
        val morPs = ProgramState.nextPs(programState)
        val newProgramState: ProgramState = morPs.right.value
        // new program shall be subProgram
        // new stack stall be old stack with old program and firstNextLine prepended
        assertProgramState(programState)(newProgramState,
                                         newCurrentLineO = Some(firstNextLine))
    }
  }

  it should "fail if next line does not exist" in new Fixture {
    val genProgramStateForPrgNoNextLine: Gen[ProgramState] = for {
      (nep, currentLine) <- genProgramNoNextLine
      subPrg <- genProgram
      program = nep.updated(currentLine, Prg(subPrg))
      programState <- genProgramState(program, currentLine)
    } yield programState

    forAll(genProgramStateForPrgNoNextLine) { programState =>
      val morPs = ProgramState.nextPs(programState)
      morPs.left.value.shouldEqual(NoNextLinenNumberFoundInProgram)
    }
  }

  behavior of "ProgramState.stream"

  trait StreamFixture extends Fixture {
    lazy val genFinishedProgramState: Gen[Mor[ProgramState]] = for {
      config <- genRegistersOpsConfig
      program <- genProgram
      registers <- genRegisters
      programState = newProgramState(config,
                                     program,
                                     emptyStack,
                                     None,
                                     registers)
    } yield Right(programState)

    lazy val nextNotToBeCalledF: ProgramState => Mor[ProgramState] = _ => fail
  }

  it should "add a Left to the stream without calling next" in new StreamFixture {
    forAll(genNonEmptyStream) { streamSoFar =>
      val newStream: Stream[Pure, Mor[ProgramState]] =
        ProgramState.stream(streamSoFar,
                            Left(MessageDuringUnitTests),
                            nextNotToBeCalledF)
      val oldSize = streamSoFar.toList.size
      val newSize = newStream.toList.size
      newSize.shouldEqual(oldSize + 1)
      newStream.take(oldSize.toLong).toList.shouldEqual(streamSoFar.toList)
      newStream.last.toList.head.shouldEqual(Some(Left(MessageDuringUnitTests)))
    }
  }

  it should "add a Right(ended program state) to the stream without calling next" in new StreamFixture {
    forAll(genNonEmptyStream, genFinishedProgramState) {
      (streamSoFar, finishedPs) =>
        val newStream: Stream[Pure, Mor[ProgramState]] =
          ProgramState.stream(streamSoFar, finishedPs, nextNotToBeCalledF)
        val oldSize = streamSoFar.toList.size
        val newSize = newStream.toList.size
        newSize.shouldEqual(oldSize + 1)
        newStream.take(oldSize.toLong).toList.shouldEqual(streamSoFar.toList)
        newStream.last.toList.head.get
          .shouldEqual(finishedPs)
    }
  }

  it should "add a Right(non-ended program state), then call the next function and add its ending program state result" in new StreamFixture {
    lazy val genEndingProgramState: Gen[Mor[ProgramState]] = for {
      morProgramState <- Gen.oneOf(genFinishedProgramState,
                                   Gen.const(Left(MessageDuringUnitTests)))
    } yield morProgramState

    forAll(genNonEmptyStream, genNonFinishedProgramState, genEndingProgramState) {
      (streamSoFar, psBefore, psLast) =>
        val nextF: ProgramState => Mor[ProgramState] = prevPs => {
          prevPs.shouldEqual(psBefore.right.get)
          psLast
        }
        val newStream: Stream[Pure, Mor[ProgramState]] =
          ProgramState.stream(streamSoFar, psBefore, nextF)
        val oldSize = streamSoFar.toList.size
        val newSize = newStream.toList.size
        newSize.shouldEqual(oldSize + 2)
        newStream.take(oldSize.toLong).toList.shouldEqual(streamSoFar.toList)
        newStream.drop(oldSize.toLong).toList.shouldEqual(List(psBefore, psLast))
    }
  }

}
