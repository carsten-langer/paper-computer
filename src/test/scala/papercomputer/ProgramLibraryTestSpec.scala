package papercomputer

import eu.timepit.refined.auto._
import org.scalacheck.Gen
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import papercomputer.ProgramLibrary._

class ProgramLibraryTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture extends CommonFixtures {
    def _PRINTOUT_DURING_TESTS_ = false

    // for property-based tests, limit the register value range the platform can hold so that it does not take
    // too long to wrap around the value space for adding negative values
    def minTestRegisterValue: RegisterValue = -50
    def maxTestRegisterValue: RegisterValue = +50
    def maxTestRegisterNumber: RegisterNumber = 20L

    val registers45870: Registers = Registers(minRegisterValue,
                                              maxRegisterValue,
                                              Map(
                                                (1L, 4L),
                                                (2L, 5L),
                                                (3L, 8L),
                                                (4L, 7L),
                                                (5L, 0L)
                                              )).right.get

    def printRegisters: ProgramState => Unit =
      if (_PRINTOUT_DURING_TESTS_)
        (ps: ProgramState) =>
          println("before next: " + ps
            .program(ps.currentLineO.get) + ", " + ps.registers.registerValues)
      else _ => ()

    // wrap the rv into the value range between minRv and maxRv inclusive
    // as this match-computer implements a modulo arithmetic
    def wrapRv(rv: RegisterValue,
               minRv: RegisterValue,
               maxRv: RegisterValue): RegisterValue = {
      val rvShifted = rv - minRv
      val modulo = maxRv - minRv + 1
      // https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorMod-long-long-
      val modRvShifted = java.lang.Math.floorMod(rvShifted, modulo)
      modRvShifted + minRv
    }

  }

  behavior of "Explicit tests of ProgramLibrary test via ProgramExecution.executeObserved"

  it should "run programZeroR1 with expected result" in new Fixture {

    val morRs: Mor[Registers] =
      ProgramExecution.executeObserved(printRegisters)(programZeroR1,
                                                       registers45870)
    morRs.right.value.registerValues(1L).shouldEqual(0)
  }

  it should "run programAdditionR1plusR2toR1 with expected result" in new Fixture {
    val morRs: Mor[Registers] = ProgramExecution
      .executeObserved(printRegisters)(
        ProgramLibrary.programAdditionR1plusR2toR1,
        registers45870)
    morRs.right.value.registerValues(1L).shouldEqual(9)
  }

  it should "run programAdditionR2plusR3toR1 with expected result" in new Fixture {
    val morRs: Mor[Registers] =
      ProgramExecution.executeObserved(printRegisters)(
        programAdditionR2plusR3toR1,
        registers45870)
    morRs.right.value.registerValues(1L).shouldEqual(13)
  }

  it should "run fibonacci with expected result" in new Fixture {
    val morRs: Mor[Registers] =
      ProgramExecution.executeObserved(printRegisters)(
        fibonacci(3L, 1L, 2L, 4L, 5L),
        registers45870)
    morRs.right.value.registerValues(1L).shouldEqual(21)
  }

  behavior of "Property-based tests of ProgramLibrary test via ProgramExecution.executeObserved"

  it should "run zeroRx with expected result" in new Fixture {
    val gen: Gen[(Registers, RegisterNumber)] = for {
      (_, _, rvs, neRs) <- genMinMaxRegisterValueRegisterValuesRegisters(
        1L,
        minTestRegisterValue,
        maxTestRegisterValue)
      rn <- Gen.oneOf(rvs.keys.toSeq)
    } yield (neRs, rn)

    forAll(gen) {
      case (rs, rn) =>
        val morRs = ProgramExecution.executeObserved(printRegisters)(
          ProgramLibrary.zeroRx(rn),
          rs)
        morRs.right.value.registerValues(rn).shouldEqual(0)
    }
  }

  it should "run raPlusRbToRa with expected result" in new Fixture {
    val gen: Gen[(Registers, RegisterNumber, RegisterNumber, RegisterValue)] =
      for {
        (minRv, maxRv, rvs, neRs) <- genMinMaxRegisterValueRegisterValuesRegisters(
          2L,
          minTestRegisterValue,
          maxTestRegisterValue)
        Seq((rnA, rvA), (rnB, rvB)) <- Gen.pick(2, rvs)
        expectedRv = wrapRv(rvA + rvB, minRv, maxRv)
      } yield (neRs, rnA, rnB, expectedRv)

    forAll(gen) {
      case (rs, rnA, rnB, expectedRv) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.raPlusRbToRa(rnA, rnB),
            rs)
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

  it should "run rbPlusRcToRa with expected results" in new Fixture {
    val gen: Gen[(Registers,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterValue)] =
      for {
        (minRv, maxRv, rvs, neRs) <- genMinMaxRegisterValueRegisterValuesRegisters(
          3L,
          minTestRegisterValue,
          maxTestRegisterValue)
        Seq((rnA, _), (rnB, rvB), (rnC, rvC)) <- Gen.pick(3, rvs)
        expectedRv = wrapRv(rvB + rvC, minRv, maxRv)
      } yield (neRs, rnA, rnB, rnC, expectedRv)

    forAll(gen) {
      case (rs, rnA, rnB, rnC, expectedRv) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.rbPlusRcToRa(rnA, rnB, rnC),
            rs)
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

  it should "run copy with expected results" in new Fixture {
    val gen: Gen[(Registers,
                  RegisterNumber,
                  RegisterValue,
                  RegisterNumber,
                  RegisterNumber)] =
      for {
        (_, _, rvs, neRs) <- genMinMaxRegisterValueRegisterValuesRegisters(
          3L,
          minTestRegisterValue,
          maxTestRegisterValue)
        Seq((fromRn, fromRv), (toRn, _), (tmpRn, _)) <- Gen.pick(3, rvs)
      } yield (neRs, fromRn, fromRv, toRn, tmpRn)

    forAll(gen) {
      case (rs, fromRn, fromRv, toRn, tmpRn) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.copy(fromRn, toRn, tmpRn),
            rs)
        morRs.right.value.registerValues(toRn).shouldEqual(fromRv)
    }
  }

  it should "run multiplyRnBWithRnCToRnA with expected results" in new Fixture {
    val gen: Gen[(Registers,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterValue)] =
      for {
        (minRv, maxRv, rvs, neRs) <- genMinMaxRegisterValueRegisterValuesRegisters(
          5L,
          minTestRegisterValue,
          maxTestRegisterValue)
        Seq((rnA, _), (rnB, rvB), (rnC, rvC), (rnT1, _), (rnT2, _)) <- Gen.pick(
          5,
          rvs)
        expectedRv = wrapRv(rvB * rvC, minRv, maxRv)
      } yield (neRs, rnA, rnB, rnC, rnT1, rnT2, expectedRv)

    forAll(gen) {
      case (rs, rnA, rnB, rnC, rnT1, rnT2, expectedRv) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.multiplyRbWithRcToRa(rnA, rnB, rnC, rnT1, rnT2),
            rs)
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

  it should "run rnAMinusRnBToRnA with expected results" in new Fixture {
    val gen: Gen[(Registers, RegisterNumber, RegisterNumber, RegisterValue)] =
      for {
        (minRv, maxRv, rvs, neRs) <- genMinMaxRegisterValueRegisterValuesRegisters(
          2L,
          minTestRegisterValue,
          maxTestRegisterValue)
        Seq((rnA, rvA), (rnB, rvB)) <- Gen.pick(2, rvs)
        expectedRv = wrapRv(rvA - rvB, minRv, maxRv)
      } yield (neRs, rnA, rnB, expectedRv)

    forAll(gen) {
      case (rs, rnA, rnB, expectedRv) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.raMinusRbToRa(rnA, rnB),
            rs)
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

}
