package papercomputer

import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import org.scalacheck.Gen
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProgramLibraryTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  def _PRINTOUT_DURING_TESTS_ = false

  trait Fixture {
    // for tests, limit the register value range the platform can hold so that it does not take
    // too long to wrap around the value space for adding negative values
    def minTestRegisterValue: RegisterValue = -50
    def maxTestRegisterValue: RegisterValue = +50
    def maxTestRegisterNumber: RegisterNumber = 20L

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

    lazy val genMinMaxRegisterValue: Gen[(RegisterValue, RegisterValue)] =
      for {
        minRv <- Gen.chooseNum[RegisterValue](minTestRegisterValue, 0)
        maxRv <- Gen.chooseNum[RegisterValue](0, maxTestRegisterValue)
      } yield (minRv, maxRv)

    lazy val genRegisterNumber: Gen[RegisterNumber] =
      Gen
        .chooseNum(minRegisterNumber.value, maxRegisterNumber.value)
        .map(refineV[NonNegative](_).right.get)

    def genMinMaxRegisterValueRegisterValues(minNumRegisters: RegisterNumber)
      : Gen[(RegisterValue, RegisterValue, RegisterValues)] =
      for {
        (minRv, maxRv) <- genMinMaxRegisterValue
        genRv = Gen.chooseNum(minRv, maxRv)
        genRnRv = Gen.zip(genRegisterNumber, genRv)
        rnRvs <- Gen.mapOf(genRnRv).suchThat(_.size >= minNumRegisters.value)
      } yield (minRv, maxRv, rnRvs)

    def genRegisters(minNumRegisters: RegisterNumber)
      : Gen[(RegisterValue, RegisterValue, RegisterValues, Registers)] =
      for {
        (minRV, maxRV, rvs) <- genMinMaxRegisterValueRegisterValues(
          minNumRegisters)
        rs = Registers(minRV, maxRV, rvs).right.get
      } yield (minRV, maxRV, rvs, rs)

    def printRegisters: ProgramState => Unit =
      if (_PRINTOUT_DURING_TESTS_)
        (ps: ProgramState) =>
          println("before next: " + ps)
      else _ => ()

  }

  behavior of "ProgramLibrary.zeroRx"

  it should "work as expected" in new Fixture {
    val gen: Gen[(Registers, RegisterNumber)] = for {
      (_, _, rvs, neRs) <- genRegisters(1L)
      rn <- Gen.oneOf(rvs.keys.toSeq)
    } yield (neRs, rn)

    forAll(gen) {
      case (rs: Registers, rn: RegisterNumber) =>
        val morRs = ProgramExecution.executeObserved(printRegisters)(ProgramLibrary.zeroRx(rn), rs)
        morRs.right.value.registerValues(rn).shouldEqual(0)
    }
  }

  behavior of "ProgramLibrary.rnAPlusRnBToRnA"

  it should "work as expected" in new Fixture {
    val gen: Gen[(Registers, RegisterNumber, RegisterNumber, RegisterValue)] =
      for {
        (minRv, maxRv, rvs, neRs) <- genRegisters(2L)
        Seq((rnA, rvA), (rnB, rvB)) <- Gen.pick(2, rvs)
        expectedRv = wrapRv(rvA + rvB, minRv, maxRv)
      } yield (neRs, rnA, rnB, expectedRv)

    forAll(gen) {
      case (rs: Registers,
            rnA: RegisterNumber,
            rnB: RegisterNumber,
            rv: RegisterValue) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.rnAPlusRnBToRnA(rnA, rnB),
            rs)
        morRs.right.value.registerValues(rnA).shouldEqual(rv)
    }
  }

  behavior of "ProgramLibrary.rbPlusRcToRa"

  it should "work as expected" in new Fixture {
    val gen: Gen[(Registers,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterNumber,
                  RegisterValue)] =
      for {
        (minRv, maxRv, rvs, neRs) <- genRegisters(3L)
        Seq((rnA, _), (rnB, rvB), (rnC, rvC)) <- Gen.pick(3, rvs)
        expectedRv = wrapRv(rvB + rvC, minRv, maxRv)
      } yield (neRs, rnA, rnB, rnC, expectedRv)

    forAll(gen) {
      case (rs: Registers,
            rnA: RegisterNumber,
            rnB: RegisterNumber,
            rnC: RegisterNumber,
            rv: RegisterValue) =>
        val morRs = ProgramExecution
          .executeObserved(printRegisters)(
            ProgramLibrary.rbPlusRcToRa(rnA, rnB, rnC),
            rs)
        morRs.right.value.registerValues(rnA).shouldEqual(rv)
    }
  }
 // todo test other programs from library
}
