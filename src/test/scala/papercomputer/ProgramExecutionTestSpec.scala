package papercomputer

import cats.data.StateT
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.EitherValues.{
  convertLeftProjectionToValuable,
  convertRightProjectionToValuable
}
import org.scalatest.{Assertions, FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProgramExecutionTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture extends CommonFixtures {
    lazy val nextShouldNotBeCalled: StateT[Mor, ProgramState, ProgramState] =
      StateT[Mor, ProgramState, ProgramState](_ =>
        Assertions.fail(" next should not be called"))

    val genProgramState: Gen[ProgramState] = for {
      (program, currentLine) <- genProgramAndAnyContainedLine
      programState <- genProgramState(program, currentLine)
    } yield programState
  }

  behavior of "ProgramExecution.toStream"

  it should "fail for Left as input" in new Fixture {
    val morProgramState #:: tail: Stream[Mor[ProgramState]] =
      ProgramExecution
        .toStream(Left(MessageDuringUnitTests), nextShouldNotBeCalled)
    morProgramState.left.value
      .shouldEqual(MessageDuringUnitTests)
    tail.isEmpty.shouldBe(true)
  }

  it should "return correct result for Right(ended program state) as input" in new Fixture {
    forAll(genRegisters) { registers: Registers =>
      val morLastPs = ProgramState(emptyProgram, registers)
      val morProgramState #:: tail: Stream[Mor[ProgramState]] =
        ProgramExecution
          .toStream(morLastPs, nextShouldNotBeCalled)
      morProgramState.right.value.shouldEqual(morLastPs.right.get)
      tail.isEmpty.shouldBe(true)
    }
  }

  it should "call next function for Right(non-ended program state), then interpret the Right result" in new Fixture {
    forAll(genProgramState, genRegisters) { (psBefore, registers) =>
      val morLastPs = ProgramState(emptyProgram, registers)
      val next: StateT[Mor, ProgramState, ProgramState] =
        StateT[Mor, ProgramState, ProgramState](ps => {
          ps.shouldEqual(psBefore)
          morLastPs.map((_, ps))
        })
      val _ #:: morProgramState #:: tail: Stream[Mor[ProgramState]] =
        ProgramExecution
          .toStream(Right(psBefore), next)
      morProgramState.right.value
        .shouldEqual(morLastPs.right.get)
      tail.isEmpty.shouldBe(true)
    }
  }

  it should "call next function for Right(non-ended program state), then interpret the Left result" in new Fixture {
    forAll(genProgramState) { psBefore =>
      val morLastPs = Left(MessageDuringUnitTests)
      val next: StateT[Mor, ProgramState, ProgramState] =
        StateT[Mor, ProgramState, ProgramState](ps => {
          ps.shouldEqual(psBefore)
          morLastPs
        })
      val _ #:: morProgramState #:: tail: Stream[Mor[ProgramState]] =
        ProgramExecution
          .toStream(Right(psBefore), next)
      morProgramState.left.value
        .shouldEqual(MessageDuringUnitTests)
      tail.isEmpty.shouldBe(true)
    }
  }

  behavior of "ProgramExecution.executeObserved"

  it should "return the original registers for a non-executable program" in new Fixture {
    forAll(genRegisters) { registers: Registers =>
      def sideEffect(ps: ProgramState): Unit = {
        val _ = ps.registers.shouldEqual(registers)
      }
      val morRegisters: Mor[Registers] = ProgramExecution
        .executeObserved(sideEffect, nextShouldNotBeCalled)(emptyProgram,
                                                            registers)
      morRegisters.right.value.shouldEqual(registers)
    }
  }

  it should "call sideEffect function for Right(non-ended program state), then call next function then interpret the Right result" in new Fixture {
    forAll(genProgramState, genRegisters) { (psBefore, registers) =>
      val sideEffect: ProgramState => Unit = {
        var numCalled: Int = 0
        (ps: ProgramState) =>
          {
            numCalled += 1
            val _ =
              if (numCalled == 1)
                ps.registers.shouldEqual(psBefore.registers)
              else
                ps.registers.shouldEqual(registers)
          }
      }
      val morLastPs = ProgramState(emptyProgram, registers)
      val next: StateT[Mor, ProgramState, ProgramState] =
        StateT[Mor, ProgramState, ProgramState](ps => {
          ps.program.shouldEqual(psBefore.program)
          ps.registers.shouldEqual(psBefore.registers)
          morLastPs.map((_, ps))
        })
      val morRegisters = ProgramExecution
        .executeObserved(sideEffect, next)(psBefore.program, psBefore.registers)
      morRegisters.right.value
        .shouldEqual(registers)
    }
  }

  it should "call sideEffect function for Right(non-ended program state), then call next function then interpret the Left result" in new Fixture {
    forAll(genProgramState) { psBefore =>
      def sideEffect(ps: ProgramState): Unit = {
        val _ = ps.registers.shouldEqual(psBefore.registers)
      }
      val failingPs = Left(MessageDuringUnitTests)
      val next: StateT[Mor, ProgramState, ProgramState] =
        StateT[Mor, ProgramState, ProgramState](ps => {
          ps.registers.shouldEqual(psBefore.registers)
          failingPs
        })
      val morRegisters = ProgramExecution
        .executeObserved(sideEffect, next)(psBefore.program, psBefore.registers)
      morRegisters.left.value
        .shouldEqual(MessageDuringUnitTests)
    }
  }

}
