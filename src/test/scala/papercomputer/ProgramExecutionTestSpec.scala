package papercomputer

import eu.timepit.refined.auto._
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProgramExecutionTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  def _PRINTOUT_DURING_TESTS_ = false

  trait Fixture {
    val registers4500: Registers = Registers(minRegisterValue,
                                             maxRegisterValue,
                                             Map(
                                               (1L, 4L),
                                               (2L, 5L),
                                               (3L, 0L),
                                               (4L, 0L)
                                             )).right.get
    val registers9234: Registers = Registers(minRegisterValue,
                                             maxRegisterValue,
                                             Map(
                                               (1L, 9L),
                                               (2L, 2L),
                                               (3L, 3L),
                                               (4L, 4L)
                                             )).right.get
    def printRegisters: ProgramState => Unit =
      if (_PRINTOUT_DURING_TESTS_)
        (ps: ProgramState) =>
          println("before next: " + ps
            .program(ps.currentLineO.get) + ", " + ps.registers.registerValues)
      else _ => ()
  }

  behavior of "ProgramExecution.execute"

  it should "return correct registers for programAdditionR1plusR2toR1" in new Fixture {

    val programAdditionR1plusR2toR1: Program = Map(
      (10L, Isz(2L)),
      (20L, Jmp(40L)),
      (30L, Stp),
      (40L, Inc(1L)),
      (50L, Dec(2L)),
      (60L, Jmp(10L))
    )

    val morRs: MorRegisters =
      ProgramExecution.executeObserved(printRegisters)(
        programAdditionR1plusR2toR1,
        registers4500)
    morRs.right.value.registerValues(1L).shouldEqual(9)
  }

  it should "return correct registers for programZeroR1" in new Fixture {

    val programZeroR1: Program = Map(
      (10L, Isz(1L)),
      (20L, Jmp(40L)),
      (30L, Stp),
      (40L, Dec(1L)),
      (50L, Jmp(10L))
    )
    val morRs: MorRegisters =
      ProgramExecution.executeObserved(printRegisters)(programZeroR1,
                                                       registers4500)
    morRs.right.value.registerValues(1L).shouldEqual(0)
  }

  it should "return correct registers for programAdditionR2plusR3toR1" in new Fixture {

    val programAdditionR2plusR3toR1: Program = Map(
      (10L, Isz(1L)),
      (20L, Jmp(40L)),
      (30L, Jmp(60L)),
      (40L, Dec(1L)),
      (50L, Jmp(10L)),
      (60L, Isz(2L)),
      (70L, Jmp(90L)),
      (80L, Jmp(120L)),
      (90L, Dec(2L)),
      (100L, Inc(1L)),
      (110L, Jmp(60L)),
      (120L, Isz(3L)),
      (130L, Jmp(150L)),
      (140L, Stp),
      (150L, Dec(3L)),
      (160L, Inc(1L)),
      (170L, Jmp(120L))
    )

    val morRs: MorRegisters =
      ProgramExecution.executeObserved(printRegisters)(
        programAdditionR2plusR3toR1,
        registers9234)
    morRs.right.value.registerValues(1L).shouldEqual(5)
  }

}
