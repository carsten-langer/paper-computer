package papercomputer

import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import org.scalacheck.Gen
import org.scalatest.EitherValues.{
  convertLeftProjectionToValuable,
  convertRightProjectionToValuable
}
import org.scalatest.{Assertion, FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class RegistersTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture {
    lazy val genMinMaxRegisterValue: Gen[(RegisterValue, RegisterValue)] =
      for {
        minRv <- Gen.chooseNum[RegisterValue](minRegisterValue, 0)
        maxRv <- Gen.chooseNum[RegisterValue](0, maxRegisterValue)
      } yield (minRv, maxRv)

    lazy val genRegisterNumber: Gen[RegisterNumber] =
      Gen
        .chooseNum(minRegisterNumber.value, maxRegisterNumber.value)
        .map(refineV[NonNegative](_).right.get)

    lazy val genMinMaxRegisterValueRegisterValues
      : Gen[(RegisterValue, RegisterValue, RegisterValues)] =
      for {
        (minRv, maxRv) <- genMinMaxRegisterValue
        genRv = Gen.chooseNum(minRv, maxRv)
        genRnRv = Gen.zip(genRegisterNumber, genRv)
        rnRvs <- Gen.mapOf(genRnRv)
      } yield (minRv, maxRv, rnRvs)

    lazy val genMinMaxRegisterValueRegisterValuesRegisters
      : Gen[(RegisterValue, RegisterValue, RegisterValues, Registers)] =
      for {
        (minRv, maxRv, rvs) <- genMinMaxRegisterValueRegisterValues
        rs = Registers(minRv, maxRv, rvs).right.get
      } yield (minRv, maxRv, rvs, rs)

    lazy val genRegisters: Gen[Registers] =
      genMinMaxRegisterValueRegisterValuesRegisters.map(_._4)

    def assertFailureForNonExistingRegisterNumber(
        functionUnderTest: RegisterNumber => Registers => Mor[_]): Assertion =
      forAll((genRegisters, "registers"), (genRegisterNumber, "registerNumber")) {
        (rs: Registers, rn: RegisterNumber) =>
          whenever(!rs.registerValues.contains(rn)) {
            val message: Mor[_] = functionUnderTest(rn)(rs)
            message.left.value
              .shouldEqual(IllegalAccessToNonExistingRegisterNumber)
          }
      }

    lazy val mockRegisters = Registers(0, 0, Map.empty)

    def assertingRegistersFactory(
        notifyCalled: => Unit,
        expectedMinRv: RegisterValue,
        expectedMaxRv: RegisterValue,
        originalRvs: RegisterValues,
        rnUnderTest: RegisterNumber,
        expectedRvForRnUnderTestF: RegisterValue => RegisterValue)
      : RegistersFactory =
      (minRv: RegisterValue, maxRv: RegisterValue, rvs: RegisterValues) => {
        notifyCalled
        minRv.shouldEqual(expectedMinRv)
        maxRv.shouldEqual(expectedMaxRv)
        originalRvs.foreach({
          case (rnToCheck: RegisterNumber, rvOrig: RegisterValue) =>
            val expectedRv =
              if (rnToCheck == rnUnderTest) expectedRvForRnUnderTestF(rvOrig)
              else rvOrig
            rvs(rnToCheck).shouldEqual(expectedRv)
        })
        mockRegisters
      }

    def assertingIncDecCheck(
        incDecF: RegistersFactory => IncDecF,
        originalRvs: RegisterValues,
        boundRv: RegisterValue,
        expectedMinRv: RegisterValue,
        expectedMaxRv: RegisterValue,
        rs: Registers,
        expectedRvForRnUnderTestF: RegisterValue => RegisterValue): Unit = {
      // make sure at least one original value is at the upper/lower bound
      whenever(originalRvs.values.exists(_ == boundRv)) {
        // for each original registers, inc/dec each register number in turn
        // and check that only this register number was inc/dec'ed and all others are kept the same
        originalRvs.keys.foreach(rnUnderTest => {
          var numCalled: Int = 0
          val registersFactory =
            assertingRegistersFactory(numCalled += 1,
                                      expectedMinRv,
                                      expectedMaxRv,
                                      originalRvs,
                                      rnUnderTest,
                                      expectedRvForRnUnderTestF)
          val morRsChanged: MorRegisters =
            incDecF(registersFactory)(rnUnderTest)(rs)
          morRsChanged.shouldEqual(mockRegisters)
          numCalled.shouldEqual(1)
        })
      }
    }
  }

  behavior of "Registers.apply"

  it should "fail if minRv > 0" in new Fixture {
    val gen: Gen[(RegisterValue, RegisterValue)] = for {
      maxRv <- Gen.chooseNum[RegisterValue](1, maxRegisterValue)
      minRv <- Gen.chooseNum[RegisterValue](1, maxRv)
    } yield (minRv, maxRv)

    forAll((gen, "minRv, maxRv")) {
      case (minRv: RegisterValue, maxRv: RegisterValue) =>
        whenever(minRv <= maxRv && minRv > 0) {
          val rvs: RegisterValues = Map.empty
          val mors: MorRegisters = Registers(minRv, maxRv, rvs)
          mors.left.value
            .shouldEqual(MinRegisterValueMustBeLessOrEqualZero)
        }
    }
  }

  it should "fail if maxRv < 0" in new Fixture {
    val gen: Gen[(RegisterValue, RegisterValue)] = for {
      minRv <- Gen.chooseNum[RegisterValue](minRegisterValue, -1)
      maxRv <- Gen.chooseNum[RegisterValue](minRv, -1)
    } yield (minRv, maxRv)

    forAll((gen, "minRv, maxRv")) {
      case (minRv: RegisterValue, maxRv: RegisterValue) =>
        whenever(minRv <= maxRv && maxRv < 0) {
          val rvs: RegisterValues = Map.empty
          val mors: MorRegisters = Registers(minRv, maxRv, rvs)
          mors.left.value
            .shouldEqual(MaxRegisterValueMustBeGreaterOrEqualZero)
        }
    }
  }

  it should "fail if any value < minRv" in new Fixture {
    val gen: Gen[(RegisterValue, RegisterValue, RegisterValues)] = for {
      (minRv, maxRv) <- genMinMaxRegisterValue
      genRvLessMin = Gen.chooseNum(minRegisterValue, minRv)
      genRnRv = Gen.zip(genRegisterNumber, genRvLessMin)
      rnrvs <- Gen.mapOf(genRnRv)
    } yield (minRv, maxRv, rnrvs)

    forAll((gen, "minRv, maxRv, registerValues")) {
      case (minRv: RegisterValue, maxRv: RegisterValue, rvs: RegisterValues) =>
        whenever(rvs.values.exists(_ < minRv)) {
          val mors: MorRegisters = Registers(minRv, maxRv, rvs)
          mors.left.value
            .shouldEqual(RegisterValueMustNotBeSmallerThanMinRegisterValue)
        }
    }
  }

  it should "fail if any value > maxRv" in new Fixture {
    val gen: Gen[(RegisterValue, RegisterValue, RegisterValues)] = for {
      (minRv, maxRv) <- genMinMaxRegisterValue
      genRvGEMin = Gen.chooseNum(minRv, maxRegisterValue)
      genRnRv = Gen.zip(genRegisterNumber, genRvGEMin)
      rnrvs <- Gen.mapOf(genRnRv)
    } yield (minRv, maxRv, rnrvs)

    forAll((gen, "minRv, maxRv, registerValues")) {
      case (minRv: RegisterValue, maxRv: RegisterValue, rvs: RegisterValues) =>
        whenever(rvs.values.exists(_ > maxRv)) {
          val mors: MorRegisters = Registers(minRv, maxRv, rvs)
          mors.left.value
            .shouldEqual(RegisterValueMustNotBeGreaterThanMaxRegisterValue)
        }
    }
  }

  it should "succeed if all values >= minRV and <= maxRV" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValues, "minRv, maxRv, registerValues")) {
      case (minRv: RegisterValue, maxRv: RegisterValue, rvs: RegisterValues) =>
        val mors: MorRegisters = Registers(minRv, maxRv, rvs)
        mors.right.value // assert it is a Right(_) and ignore the values as they are tested in a different test
    }
  }

  behavior of "Registers.minRegisterValue"

  it should "return correct value" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesRegisters,
       "minRv, maxRv, registerValues, registers")) {
      case (minRv, _, _, rs: Registers) =>
        rs.minRegisterValue.shouldEqual(minRv)
    }
  }

  behavior of "Registers.maxRegisterValue"

  it should "return correct value" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesRegisters,
       "minRv, maxRv, registerValues, registers")) {
      case (_, maxRv, _, rs: Registers) =>
        rs.maxRegisterValue.shouldEqual(maxRv)
    }
  }

  behavior of "Registers.registerValues"

  it should "return correct values" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesRegisters,
       "minRv, maxRv, registerValues, registers")) {
      case (_, _, rvs, rs: Registers) =>
        rs.registerValues.shouldEqual(rvs)
    }
  }

  /* TODO re-enable tests for Registers.registerValue if it becomes public again
     A I made Registers.registerValue private and thus not part of the public API anymore,
     it cannot be tested. I keep the tests if ever I want to make it part of the public API again.
  behavior of "Registers.registerValue"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(Registers.registerValue)
  }

  it should "return correct value" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      rs.registerValues.foreach({
        case (rn, rv) =>
          Registers.registerValue(rn)(rs).right.value.shouldEqual(rv)
      })
    }
  }
  */

  behavior of "Registers.isZero"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(Registers.isZero)
  }

  it should "return correct value" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      val rvs = rs.registerValues
      whenever(rvs.values.exists(_ == 0) && rvs.values.exists(_ != 0)) {
        rvs.foreach({
          case (rn, rv) =>
            Registers.isZero(rn)(rs).right.value.shouldEqual(rv == 0)
        })
      }

    }
  }

  behavior of "Registers.inc"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(Registers.inc(Registers.apply))
  }

  it should "inc register incl. wrap around if needed, call RegistersFactory with right values" +
    " and return the new registers object" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      val minRv = rs.minRegisterValue
      val maxRv = rs.maxRegisterValue
      assertingIncDecCheck(Registers.inc,
                           rs.registerValues,
                           boundRv = maxRv,
                           minRv,
                           maxRv,
                           rs,
                           rvOrig => if (rvOrig == maxRv) minRv else rvOrig + 1)
    }
  }

  behavior of "Registers.dec"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(Registers.dec(Registers.apply))
  }

  it should "dec register incl. wrap around if needed, call RegistersFactory with right values" +
    " and return the new registers object" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      val minRv = rs.minRegisterValue
      val maxRv = rs.maxRegisterValue
      assertingIncDecCheck(Registers.dec,
                           rs.registerValues,
                           boundRv = minRv,
                           minRv,
                           maxRv,
                           rs,
                           rvOrig => if (rvOrig == minRv) maxRv else rvOrig - 1)
    }
  }
}
