package matchcomputer

import org.scalacheck.Gen
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CommandTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  trait Fixture {
    lazy val mockRs: Registers = Registers(0, 0, Map.empty).right.value
  }

  behavior of "Command.incDecF"

  it should "call registers2MorRegistersF with correct registers and return its returned message" +
    " or registers and return continueWithNextLine" in new Fixture {
    val mockRsToReturn: Registers = Registers(1, 1, Map.empty).right.value
    val ml = Left(MessageDuringUnitTests)
    val mr = Right(mockRsToReturn)
    val morsGen: Gen[MorRegisters] = Gen.oneOf(ml, mr)
    forAll((morsGen, "messageOrRegisters")) { genMors: MorRegisters =>

      var numCalls = 0
      val rs2morsF: Registers2MorRegistersF =
        (rs: Registers) => {
          numCalls += 1
          rs.shouldEqual(mockRs)
          genMors
        }

      val incDecCommand: CommandF = Command.incDecF(rs2morsF)
      val mors: MorRegistersProgramControl = incDecCommand(mockRs)

      mors match {
        case Left(_) => mors.shouldEqual(genMors)
        case Right((rs, pc)) =>
          rs.shouldEqual(mockRsToReturn)
          pc.shouldEqual(ContinueWithNextLine)
      }
      numCalls.shouldEqual(1)
    }
  }

  behavior of "Command.jmp"

  it should "not change the registers and send ContinueWithLine with correct line" in new Fixture
  with CommonFixtures.LineLumberGenerator {

    forAll((lineGen, "linenumber")) { lineNumber: LineNumber =>
      val command: CommandF = Command.jmp(lineNumber)
      val morspc: MorRegistersProgramControl = command(mockRs)

      morspc.right.value._1.shouldEqual(mockRs)
      morspc.right.value._2.shouldEqual(ContinueWithLine(lineNumber))
    }
  }

  behavior of "Command.iszF"

  it should "call registers2MorBooleanF with correct registers and return its returned message" +
    " or return continueWith(Second)NextLine depending on returned value" in new Fixture {
    val ml = Left(MessageDuringUnitTests)
    val mrt = Right(true)
    val mrf = Right(false)
    val morbGen: Gen[MorBoolean] = Gen.oneOf(ml, mrt, mrf)
    forAll((morbGen, "messageOrBoolean")) { genMorb: MorBoolean =>
      var numCalls = 0
      def mockRegisters2MorBooleanF: Registers => MorBoolean =
        (rs: Registers) => {
          numCalls += 1
          rs.shouldEqual(mockRs)
          genMorb
        }

      val iszCommand: CommandF = Command.iszF(mockRegisters2MorBooleanF)
      val mors: MorRegistersProgramControl = iszCommand(mockRs)

      (genMorb, mors) match {
        case (Left(m1), Left(m2)) => m1.shouldEqual(m2)
        case (Right(true), Right((rs, pc))) =>
          rs.shouldEqual(mockRs)
          pc.shouldEqual(ContinueWithSecondNextLine)
        case (Right(false), Right((rs, pc))) =>
          rs.shouldEqual(mockRs)
          pc.shouldEqual(ContinueWithNextLine)
        case _ => fail("should not happen")
      }
      numCalls.shouldEqual(1)
    }
  }

  behavior of "Command.sub"

  ignore should "sub not yet implemented" in {
    // TODO implement sub test
  }

  behavior of "Command.stp"

  it should "not change the registers and send Stop" in new Fixture {
    val command: CommandF = Command.stp
    val result: MorRegistersProgramControl = command(mockRs)
    result.right.value._1.shouldEqual(mockRs)
    result.right.value._2.shouldEqual(Stop)
  }
}
