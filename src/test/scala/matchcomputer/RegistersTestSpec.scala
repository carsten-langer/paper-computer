package matchcomputer

import org.scalacheck.{Arbitrary, Gen}
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

  trait Fixture extends CommonFixtures.RegisterNumberGenerator {
    def genArbitraryRegisterValue: Gen[RegisterValue] =
      Arbitrary.arbitrary[RegisterValue]

    def genMinMaxRegisterValueTuple: Gen[(RegisterValue, RegisterValue)] =
      for {
        minRV <- genArbitraryRegisterValue
        maxRV <- Gen.chooseNum(minRV, maxRegisterValue)
      } yield (minRV, maxRV)

    def genMinMaxRegisterValueRegisterValuesTuple
      : Gen[(RegisterValue, RegisterValue, RegisterValues)] =
      for {
        (minRV, maxRV) <- genMinMaxRegisterValueTuple
        genRV = Gen.chooseNum(minRV, maxRV)
        genRnRv = Gen.zip(genRegisterNumber, genRV)
        rnRvs <- Gen.mapOf(genRnRv)
      } yield (minRV, maxRV, rnRvs)

    def genMinMaxRegisterValueRegisterValuesRegistersTuple
      : Gen[(RegisterValue, RegisterValue, RegisterValues, Registers)] =
      for {
        (minRV, maxRV, rvs) <- genMinMaxRegisterValueRegisterValuesTuple
        mors = Registers(minRV, maxRV, rvs)
        rs <- mors match {
          case Right(rs) => Gen.const(rs)
          case Left(_)   => Gen.fail
        }
      } yield (minRV, maxRV, rvs, rs)

    def genRegisters: Gen[Registers] =
      genMinMaxRegisterValueRegisterValuesRegistersTuple.map(_._4)

    def assertFailureForNonexistingRegisterNumber(
        functionUnderTest: RegisterNumber => Registers => Mor[_]): Assertion =
      forAll((genRegisters, "registers"), (genRegisterNumber, "registerNumber")) {
        (rs: Registers, rn: RegisterNumber) =>
          whenever(!rs.registerValues.contains(rn)) {
            val message: Mor[_] = functionUnderTest(rn)(rs)
            message.left.value
              .shouldEqual(IllegalAccessToNonexistingRegisterNumber)
          }
      }
  }

  behavior of "Registers.apply"

  it should "fail if minRV > maxRV" in new Fixture {
    forAll((genArbitraryRegisterValue, "minRV"),
           (genArbitraryRegisterValue, "maxRV")) {
      (minRV: RegisterValue, maxRV: RegisterValue) =>
        whenever(minRV > maxRV) {
          val rvs: RegisterValues = Map.empty
          val mors: MorRegisters = Registers(minRV, maxRV, rvs)
          mors.left.value
            .shouldEqual(MinRegisterValueMustBeLessOrEqualMaxRegisterValue)
        }
    }
  }

  it should "fail if any value < minRV" in new Fixture {
    val genArbitraryRegisterValues: Gen[RegisterValues] = for {
      rnRv <- Gen.zip(genRegisterNumber, genArbitraryRegisterValue)
      rvs <- Gen.mapOf(rnRv)
    } yield rvs

    forAll((genMinMaxRegisterValueTuple, "minRV, maxRV"),
           (genArbitraryRegisterValues, "registerValues")) {
      case ((minRV: RegisterValue, maxRV: RegisterValue),
            rvs: RegisterValues) =>
        whenever(rvs.exists(_._2 < minRV)) {
          val mors: MorRegisters = Registers(minRV, maxRV, rvs)
          mors.left.value
            .shouldEqual(RegisterValueMustNotBeSmallerThanMinRegisterValue)
        }
    }
  }

  it should "fail if any value > maxRV" in new Fixture {
    val genMinMaxRegisterValueRegisterValuesGreaterEqualMinTuple
      : Gen[(RegisterValue, RegisterValue, RegisterValues)] = for {
      (minRV, maxRV) <- genMinMaxRegisterValueTuple
      rn <- genRegisterNumber
      rvGEMin <- Gen.chooseNum(minRV, maxRegisterValue)
      rnrv <- Gen.zip(rn, rvGEMin)
      rnrvs <- Gen.mapOf(rnrv)
    } yield (minRV, maxRV, rnrvs)

    forAll(
      (genMinMaxRegisterValueRegisterValuesGreaterEqualMinTuple,
       "minRV, MaxRV, registerValues")) {
      case (minRV: RegisterValue, maxRV: RegisterValue, rvs: RegisterValues) =>
        whenever(rvs.exists(_._2 > maxRV)) {
          val mors: MorRegisters = Registers(minRV, maxRV, rvs)
          mors.left.value
            .shouldEqual(RegisterValueMustNotBeGreaterThanMaxRegisterValue)
        }
    }
  }

  it should "succeed if all values >= minRV and <= maxRV" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesTuple,
       "minRV, MaxRV, registerValues")) {
      case (minRV: RegisterValue, maxRV: RegisterValue, rvs: RegisterValues) =>
        val mors: MorRegisters = Registers(minRV, maxRV, rvs)
        mors.right.value // assert it is a Right(_)
    }
  }

  behavior of "Registers.minRegisterValue"

  it should "return correct value" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesRegistersTuple,
       "minRV, MaxRV, registerValues, registers")) {
      case (minRV: RegisterValue, _, _, rs: Registers) =>
        rs.minRegisterValue.shouldEqual(minRV)
    }
  }

  behavior of "Registers.maxRegisterValue"

  it should "return correct value" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesRegistersTuple,
       "minRV, MaxRV, registerValues, registers")) {
      case (_, maxRV: RegisterValue, _, rs: Registers) =>
        rs.maxRegisterValue.shouldEqual(maxRV)
    }
  }

  behavior of "Registers.registerValues"

  it should "return correct values" in new Fixture {
    forAll(
      (genMinMaxRegisterValueRegisterValuesRegistersTuple,
       "minRV, MaxRV, registerValues, registers")) {
      case (_, _, rvs: RegisterValues, rs: Registers) =>
        rs.registerValues.shouldEqual(rvs)
    }
  }

  behavior of "Registers.registerValue"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonexistingRegisterNumber(Registers.registerValue)
  }

  it should "return correct value" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      rs.registerValues.foreach({
        case (rn, rv) =>
          Registers.registerValue(rn)(rs).right.value.shouldEqual(rv)
      })
    }
  }

  behavior of "Registers.isZero"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonexistingRegisterNumber(Registers.isZero)
  }

  it should "return correct value" in new Fixture {
    forAll((genRegisters, "registers")) { rs: Registers =>
      {
        val rvs = rs.registerValues
        whenever(rvs.exists(_._2 == 0) && rvs.exists(_._2 != 0)) {
          rvs.foreach({
            case (rn, rv) =>
              Registers.isZero(rn)(rs).right.value.shouldEqual(rv == 0)
          })
        }
      }
    }
  }

  behavior of "Registers.inc"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonexistingRegisterNumber(Registers.inc(Registers.apply))
  }

  it should "inc register incl. wrap around if needed, call RegistersFactory with right values" +
    " and return the new registers object" in new Fixture {
    val mockRegisters = Registers(0, 0, Map.empty)
    forAll((genRegisters, "registers")) { rs: Registers =>
      {
        val originalMinRV = rs.minRegisterValue
        val originalMaxRV = rs.maxRegisterValue
        val originalRvs = rs.registerValues
        whenever(originalRvs.exists(_._2 == originalMaxRV)) {
          // for each original registers, increment each register number in turn
          // and check that only this register number was incremented and all others are kept the same
          originalRvs.keys.foreach(rnUnderTest => {
            var numCalled: Int = 0

            def mockRegistersFactory: RegistersFactory =
              (minRV: RegisterValue,
               maxRV: RegisterValue,
               rvs: RegisterValues) => {
                numCalled += 1
                minRV.shouldEqual(originalMinRV)
                maxRV.shouldEqual(originalMaxRV)
                originalRvs.foreach({
                  case (rnToCheck: RegisterNumber, rvOrig: RegisterValue) =>
                    val expectedRv =
                      if (rnToCheck == rnUnderTest)
                        if (rvOrig == maxRV) minRV else rvOrig + 1
                      else rvOrig
                    rvs(rnToCheck).shouldEqual(expectedRv)
                })
                mockRegisters
              }

            val morsInced: MorRegisters =
              Registers.inc(mockRegistersFactory)(rnUnderTest)(rs)
            morsInced.shouldEqual(mockRegisters)
            numCalled.shouldEqual(1)
          })
        }
      }
    }
  }

  behavior of "Registers.dec"

  it should "fail for non-existing register number" in new Fixture {
    assertFailureForNonexistingRegisterNumber(Registers.dec(Registers.apply))
  }

  it should "dec register incl. wrap around if needed, call RegistersFactory with right values" +
    " and return the new registers object" in new Fixture {
    val mockRegisters = Registers(0, 0, Map.empty)
    forAll((genRegisters, "registers")) { rs: Registers =>
      {
        val originalMinRV = rs.minRegisterValue
        val originalMaxRV = rs.maxRegisterValue
        val originalRvs = rs.registerValues
        whenever(originalRvs.exists(_._2 == originalMinRV)) {
          // for each original registers, decrement each register number in turn
          // and check that only this register number was incremented and all others are kept the same
          originalRvs.keys.foreach(rnUnderTest => {
            var numCalled: Int = 0

            def mockRegistersFactory: RegistersFactory =
              (minRV: RegisterValue,
               maxRV: RegisterValue,
               rvs: RegisterValues) => {
                numCalled += 1
                minRV.shouldEqual(originalMinRV)
                maxRV.shouldEqual(originalMaxRV)
                originalRvs.foreach({
                  case (rnToCheck: RegisterNumber, rvOrig: RegisterValue) =>
                    val expectedRv =
                      if (rnToCheck == rnUnderTest)
                        if (rvOrig == minRV) maxRV else rvOrig - 1
                      else rvOrig
                    rvs(rnToCheck).shouldEqual(expectedRv)
                })
                mockRegisters
              }

            val morsInced: MorRegisters =
              Registers.dec(mockRegistersFactory)(rnUnderTest)(rs)
            morsInced.shouldEqual(mockRegisters)
            numCalled.shouldEqual(1)
          })
        }
      }
    }
  }
}
