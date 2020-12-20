package papercomputer

import eu.timepit.refined.auto.autoRefineV
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.EitherValues.{convertLeftProjectionToValuable, convertRightProjectionToValuable}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class RegistersTestSpec
  extends AnyFlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture extends CommonFixtures {
    def assertFailureForNonExistingRegisterNumber(functionUnderTest: RegisterNumber => Registers => Mor[_]): Assertion =
      forAll((genRegisters, "registers"), (genRegisterNumber, "registerNumber")) {
        (rs: Registers, rn: RegisterNumber) =>
          whenever(!rs.registerValues.contains(rn)) {
            val message: Mor[_] = functionUnderTest(rn)(rs)
            message.left.value.shouldEqual(IllegalAccessToNonExistingRegisterNumber)
          }
      }

    def assertingIncDecCheck(originalRs: Registers,
                             incDecF: IncDecF,
                             boundRv: RegisterValue,
                             expectedRvForRnUnderTestF: RegisterValue => RegisterValue): Unit = {
      // make sure at least one original value is at the upper/lower bound
      whenever(originalRs.registerValues.values.exists(_ == boundRv)) {
        // for each original registers, inc/dec each register number in turn
        // and check that only this register number was inc/dec'ed and all others are kept the same
        originalRs.registerValues.keys.foreach(rnUnderTest => {
          val morNewRs: Mor[Registers] = incDecF(rnUnderTest)(originalRs)
          val newRs: Registers = morNewRs.right.value
          newRs.minRegisterValue.shouldEqual(originalRs.minRegisterValue)
          newRs.maxRegisterValue.shouldEqual(originalRs.maxRegisterValue)
          originalRs.registerValues.foreach({
            case (rnToCheck: RegisterNumber, rvOrig: RegisterValue) =>
              val expectedRv =
                if (rnToCheck == rnUnderTest) expectedRvForRnUnderTestF(rvOrig)
                else rvOrig
              newRs.registerValues(rnToCheck).shouldEqual(expectedRv)
          })
        })
      }
    }
  }

  behavior of "Registers.fromRegistersConfig"

  it should "fail if minRv > 0" in new Fixture {
    val gen: Gen[RegistersConfig] = for {
      maxRv <- Gen.chooseNum[RegisterValue](1, maxRegisterValue)
      minRv <- Gen.chooseNum[RegisterValue](1, maxRv)
      genRv = Gen.chooseNum(minRv, maxRv)
      genRnRv = Gen.zip(genRegisterNumber, genRv)
      rvs <- Gen.mapOf(genRnRv)
      rsConfig = RegistersConfig(minRv, maxRv, rvs)
    } yield rsConfig

    forAll(gen) { rsConfig: RegistersConfig =>
      val mors: Mor[Registers] = Registers.fromRegistersConfig(rsConfig)
      mors.left.value.shouldEqual(MinRegisterValueMustBeLessOrEqualZero)
    }
  }

  it should "fail if maxRv < 0" in new Fixture {
    val gen: Gen[RegistersConfig] = for {
      minRv <- Gen.chooseNum[RegisterValue](minRegisterValue, -1)
      maxRv <- Gen.chooseNum[RegisterValue](minRv, -1)
      genRv = Gen.chooseNum(minRv, maxRv)
      genRnRv = Gen.zip(genRegisterNumber, genRv)
      rvs <- Gen.mapOf(genRnRv)
      rsConfig = RegistersConfig(minRv, maxRv, rvs)
    } yield rsConfig

    forAll(gen) { rsConfig: RegistersConfig =>
      val mors: Mor[Registers] = Registers.fromRegistersConfig(rsConfig)
      mors.left.value.shouldEqual(MaxRegisterValueMustBeGreaterOrEqualZero)
    }
  }

  it should "fail if any value < minRv" in new Fixture {
    val gen: Gen[RegistersConfig] = for {
      (minRv, maxRv, rvs) <- genMinMaxRegisterValueRegisterValues(0L)
      rvLessMin <- Gen.chooseNum(minRegisterValue, minRv).suchThat(_ < minRv)
      rnLessMin <- genRegisterNumber
      rvsWithLessMin = rvs + (rnLessMin -> rvLessMin)
      rsConfig = RegistersConfig(minRv, maxRv, rvsWithLessMin)
    } yield rsConfig

    forAll(gen) { rsConfig =>
      val mors: Mor[Registers] = Registers.fromRegistersConfig(rsConfig)
      mors.left.value.shouldEqual(RegisterValueMustNotBeSmallerThanMinRegisterValue)
    }
  }

  it should "fail if any value > maxRv" in new Fixture {
    val gen: Gen[RegistersConfig] = for {
      (minRv, maxRv, rvs) <- genMinMaxRegisterValueRegisterValues(0L)
      rvGreaterMax <- Gen.chooseNum(maxRv, maxRegisterValue).suchThat(_ > maxRv)
      rnGreaterMax <- genRegisterNumber
      rvsWithGreaterMax = rvs + (rnGreaterMax -> rvGreaterMax)
      rsConfig = RegistersConfig(minRv, maxRv, rvsWithGreaterMax)
    } yield rsConfig

    forAll(gen) { rsConfig =>
      val mors: Mor[Registers] = Registers.fromRegistersConfig(rsConfig)
      mors.left.value.shouldEqual(RegisterValueMustNotBeGreaterThanMaxRegisterValue)
    }
  }

  it should "succeed if all values >= minRV and <= maxRV and have copied all values in" in new Fixture {
    val gen: Gen[RegistersConfig] = for {
      (minRv, maxRv, rvs) <- genMinMaxRegisterValueRegisterValues(0L)
      rsConfig = RegistersConfig(minRv, maxRv, rvs)
    } yield rsConfig
    forAll(gen) { rsConfig =>
      val mors: Mor[Registers] = Registers.fromRegistersConfig(rsConfig)
      val rs = mors.right.value // assert it is a Right(_) and ignore the values as they are tested in a different test
      rs.minRegisterValue.shouldEqual(rsConfig.minRegisterValue)
      rs.maxRegisterValue.shouldEqual(rsConfig.maxRegisterValue)
      rs.registerValues.shouldEqual(rsConfig.registerValues)
    }
  }

  behavior of "RegistersOps.inc"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(RegistersOps.inc)
  }

  it should "inc register incl. wrap around if needed, call RegistersFactory with right values" +
    " and return the new registers object" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      val minRv = rs.minRegisterValue
      val maxRv = rs.maxRegisterValue
      assertingIncDecCheck(rs, RegistersOps.inc, boundRv = maxRv, rvOrig => if (rvOrig == maxRv) minRv else rvOrig + 1)
    }
  }

  behavior of "RegistersOps.dec"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(RegistersOps.dec)
  }

  it should "dec register incl. wrap around if needed, call RegistersFactory with right values" +
    " and return the new registers object" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      val minRv = rs.minRegisterValue
      val maxRv = rs.maxRegisterValue
      assertingIncDecCheck(rs, RegistersOps.dec, boundRv = minRv, rvOrig => if (rvOrig == minRv) maxRv else rvOrig - 1)
    }
  }

  behavior of "RegistersOps.isz"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonExistingRegisterNumber(RegistersOps.isz)
  }

  it should "return correct value" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      val rvs = rs.registerValues
      whenever(rvs.values.exists(_ == 0) && rvs.values.exists(_ != 0)) {
        rvs.foreach({
          case (rn, rv) => RegistersOps.isz(rn)(rs).right.value.shouldEqual(rv == 0)
        })
      }
    }
  }

}
