package papercomputer

import cats.data.StateT
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.EitherValues.{convertLeftProjectionToValuable, convertRightProjectionToValuable}
import org.scalatest.{Assertions, FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProgramExecutionTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture extends CommonFixtures {
    lazy val beforeNextShouldNotBeCalled: ProgramState => Unit = _ =>
      Assertions.fail("beforeNext should not be called")

    lazy val nextShouldNotBeCalled: StateT[Mor, ProgramState, ProgramState] = StateT[Mor, ProgramState, ProgramState](_ =>
      Assertions.fail(" next should not be called"))

    val genProgramState: Gen[ProgramState] = for {
      (program, currentLine) <- genProgramAndAnyContainedLine
      programState <- genProgramState(program, currentLine)
    } yield programState
  }

  behavior of "ProgramExecution.nextTailRec"

  it should "fail for Left as input" in new Fixture {
    val morRegisters: Mor[Registers] = ProgramExecution
      .nextTailRec(beforeNextShouldNotBeCalled,
                   nextShouldNotBeCalled,
                   Left(MessageDuringUnitTests))
    morRegisters.left.value
      .shouldEqual(MessageDuringUnitTests)

  }

  it should "return correct result for Right(ended program state) as input" in new Fixture {
    forAll(genRegisters) { registers: Registers =>
      val morLastPs = ProgramState(emptyProgram, registers)
      val morRegisters: Mor[Registers] = ProgramExecution
        .nextTailRec(beforeNextShouldNotBeCalled,
                     nextShouldNotBeCalled,
                     morLastPs)
      morRegisters.right.value.shouldEqual(registers)
    }
  }

  it should "call beforeNext function for Right(non-ended program state), then call next function then interpret the Right result" in new Fixture {
    forAll(genProgramState, genRegisters) { (psBefore, registers) =>
      def beforeNext(ps: ProgramState): Unit = {
        val _ = ps.shouldEqual(psBefore)
      }
      val morLastPs = ProgramState(emptyProgram, registers)
      val next: StateT[Mor, ProgramState, ProgramState] = StateT[Mor, ProgramState, ProgramState](ps => {
        ps.shouldEqual(psBefore)
        morLastPs.map((_, ps))
      })
      val morRegisters = ProgramExecution
        .nextTailRec(beforeNext, next, Right(psBefore))
      morRegisters.right.value
        .shouldEqual(registers)
    }
  }

  it should "call beforeNext function for Right(non-ended program state), then call next function then interpret the Left result" in new Fixture {
    forAll(genProgramState) { psBefore =>
      def beforeNext(ps: ProgramState): Unit = {
        val _ = ps.shouldEqual(psBefore)
      }
      val morLastPs = Left(MessageDuringUnitTests)
      val next: StateT[Mor, ProgramState, ProgramState] = StateT[Mor, ProgramState, ProgramState](ps => {
        ps.shouldEqual(psBefore)
        morLastPs
      })
      val morRegisters = ProgramExecution
        .nextTailRec(beforeNext, next, Right(psBefore))
      morRegisters.left.value
        .shouldEqual(MessageDuringUnitTests)
    }
  }

}
