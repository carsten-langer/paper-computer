package papercomputer

import cats.effect.IO
import eu.timepit.refined.auto.autoRefineV
import org.scalacheck.Gen
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

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

    val registerValues45870: RegisterValues =
      Map(
        (1L, 4L),
        (2L, 5L),
        (3L, 8L),
        (4L, 7L),
        (5L, 0L)
      )

    val registersConfig45780: RegistersConfig =
      RegistersConfig(minRegisterValue, maxRegisterValue, registerValues45870)

    def printRegisters(implicit doPrint: Boolean = _PRINTOUT_DURING_TESTS_)
      : Mor[ProgramState] => IO[Mor[Registers]] =
      morPs =>
        IO(morPs.map(ps => {
          if (doPrint)
            println(
              "command: " +
                ps.currentLineO
                  .map(ps.program) + ", register values: " + ps.registers.registerValues)
          ps.registers
        }))

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

  behavior of "Explicit tests of ProgramLibrary test via ProgramExecution.execute"

  it should "run programZeroR1 with expected result" in new Fixture {

    val morRs: Mor[Registers] =
      ProgramExecution
        .execute(ProgramLibrary.programZeroR1, registersConfig45780, printRegisters(true))
        .unsafeRunSync()
    morRs.right.value.registerValues(1L).shouldEqual(0)
  }

  it should "run programAdditionR1plusR2toR1 with expected result" in new Fixture {
    val morRs: Mor[Registers] = ProgramExecution
      .execute(ProgramLibrary.programAdditionR1plusR2toR1,
               registersConfig45780,
               printRegisters(true))
      .unsafeRunSync()
    morRs.right.value.registerValues(1L).shouldEqual(9)
  }

  it should "run programAdditionR2plusR3toR1 with expected result" in new Fixture {
    val morRs: Mor[Registers] =
      ProgramExecution
        .execute(ProgramLibrary.programAdditionR2plusR3toR1,
                 registersConfig45780,
                 printRegisters(true))
        .unsafeRunSync()
    morRs.right.value.registerValues(1L).shouldEqual(13)
  }

  it should "run fibonacci with expected result" in new Fixture {
    val morRs: Mor[Registers] =
      ProgramExecution
        .execute(ProgramLibrary.fibonacci(3L, 1L, 2L, 4L, 5L),
                 registersConfig45780,
                 printRegisters(true))
        .unsafeRunSync()
    morRs.right.value.registerValues(1L).shouldEqual(21)
  }

  behavior of "Property-based tests of ProgramLibrary test via ProgramExecution.execute"

  it should "run zeroRx with expected result" in new Fixture {
    val gen: Gen[(RegistersConfig, RegisterNumber)] = for {
      (minRv, maxRv, rvs, _) <- genMinMaxRegisterValueRegisterValuesRegisters(
        1L,
        minTestRegisterValue,
        maxTestRegisterValue)
      rsConfig = RegistersConfig(minRv, maxRv, rvs)
      rn <- Gen.oneOf(rvs.keys.toSeq)
    } yield (rsConfig, rn)

    forAll(gen) {
      case (rsConfig, rn) =>
        val morRs = ProgramExecution
          .execute(ProgramLibrary.zeroRx(rn), rsConfig, printRegisters)
          .unsafeRunSync()
        morRs.right.value.registerValues(rn).shouldEqual(0)
    }
  }

  it should "run raPlusRbToRa with expected result" in new Fixture {
    val gen
      : Gen[(RegistersConfig, RegisterNumber, RegisterNumber, RegisterValue)] =
      for {
        (minRv, maxRv, rvs, _) <- genMinMaxRegisterValueRegisterValuesRegisters(
          2L,
          minTestRegisterValue,
          maxTestRegisterValue)
        rsConfig = RegistersConfig(minRv, maxRv, rvs)
        Seq((rnA, rvA), (rnB, rvB)) <- Gen.pick(2, rvs)
        expectedRv = wrapRv(rvA + rvB, minRv, maxRv)
      } yield (rsConfig, rnA, rnB, expectedRv)

    forAll(gen) {
      case (rsConfig, rnA, rnB, expectedRv) =>
        val morRs = ProgramExecution
          .execute(ProgramLibrary.raPlusRbToRa(rnA, rnB),
                   rsConfig,
                   printRegisters)
          .unsafeRunSync()
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

  it should "run rbPlusRcToRa with expected results" in new Fixture {
    val gen: Gen[(RegistersConfig,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterValue)] =
      for {
        (minRv, maxRv, rvs, _) <- genMinMaxRegisterValueRegisterValuesRegisters(
          3L,
          minTestRegisterValue,
          maxTestRegisterValue)
        rsConfig = RegistersConfig(minRv, maxRv, rvs)
        Seq((rnA, _), (rnB, rvB), (rnC, rvC)) <- Gen.pick(3, rvs)
        expectedRv = wrapRv(rvB + rvC, minRv, maxRv)
      } yield (rsConfig, rnA, rnB, rnC, expectedRv)

    forAll(gen) {
      case (rsConfig, rnA, rnB, rnC, expectedRv) =>
        val morRs = ProgramExecution
          .execute(ProgramLibrary.rbPlusRcToRa(rnA, rnB, rnC),
                   rsConfig,
                   printRegisters)
          .unsafeRunSync()
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

  it should "run copy with expected results" in new Fixture {
    val gen: Gen[(RegistersConfig,
                  RegisterNumber,
                  RegisterValue,
                  RegisterNumber,
                  RegisterNumber)] =
      for {
        (minRv, maxRv, rvs, _) <- genMinMaxRegisterValueRegisterValuesRegisters(
          3L,
          minTestRegisterValue,
          maxTestRegisterValue)
        rsConfig = RegistersConfig(minRv, maxRv, rvs)
        Seq((fromRn, fromRv), (toRn, _), (tmpRn, _)) <- Gen.pick(3, rvs)
      } yield (rsConfig, fromRn, fromRv, toRn, tmpRn)

    forAll(gen) {
      case (rsConfig, fromRn, fromRv, toRn, tmpRn) =>
        val morRs = ProgramExecution
          .execute(ProgramLibrary.copy(fromRn, toRn, tmpRn),
                   rsConfig,
                   printRegisters)
          .unsafeRunSync()
        morRs.right.value.registerValues(toRn).shouldEqual(fromRv)
    }
  }

  it should "run multiplyRnBWithRnCToRnA with expected results" in new Fixture {
    val gen: Gen[(RegistersConfig,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterValue)] =
      for {
        (minRv, maxRv, rvs, _) <- genMinMaxRegisterValueRegisterValuesRegisters(
          5L,
          minTestRegisterValue,
          maxTestRegisterValue)
        rsConfig = RegistersConfig(minRv, maxRv, rvs)
        Seq((rnA, _), (rnB, rvB), (rnC, rvC), (rnT1, _), (rnT2, _)) <- Gen.pick(
          5,
          rvs)
        expectedRv = wrapRv(rvB * rvC, minRv, maxRv)
      } yield (rsConfig, rnA, rnB, rnC, rnT1, rnT2, expectedRv)

    forAll(gen) {
      case (rsConfig, rnA, rnB, rnC, rnT1, rnT2, expectedRv) =>
        val morRs = ProgramExecution
          .execute(
            ProgramLibrary.multiplyRbWithRcToRa(rnA, rnB, rnC, rnT1, rnT2),
            rsConfig,
            printRegisters)
          .unsafeRunSync()
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }

  it should "run rnAMinusRnBToRnA with expected results" in new Fixture {
    val gen
      : Gen[(RegistersConfig, RegisterNumber, RegisterNumber, RegisterValue)] =
      for {
        (minRv, maxRv, rvs, _) <- genMinMaxRegisterValueRegisterValuesRegisters(
          2L,
          minTestRegisterValue,
          maxTestRegisterValue)
        rsConfig = RegistersConfig(minRv, maxRv, rvs)
        Seq((rnA, rvA), (rnB, rvB)) <- Gen.pick(2, rvs)
        expectedRv = wrapRv(rvA - rvB, minRv, maxRv)
      } yield (rsConfig, rnA, rnB, expectedRv)

    forAll(gen) {
      case (rsConfig, rnA, rnB, expectedRv) =>
        val morRs = ProgramExecution
          .execute(ProgramLibrary.raMinusRbToRa(rnA, rnB),
                   rsConfig,
                   printRegisters)
          .unsafeRunSync()
        morRs.right.value.registerValues(rnA).shouldEqual(expectedRv)
    }
  }
}
