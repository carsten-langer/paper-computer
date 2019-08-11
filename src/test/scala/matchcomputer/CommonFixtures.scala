package matchcomputer

import eu.timepit.refined.numeric.{NonNegative, Positive}
import eu.timepit.refined.refineV
import org.scalacheck.Gen

object CommonFixtures {

  trait LineLumberGenerator {
    def lineGen: Gen[LineNumber] =
      Gen
        .posNum[Value]
        .map(refineV[Positive](_))
        .flatMap {
          case Right(ln) => Gen.const(ln)
          case Left(_)   => Gen.fail
        }
  }

  trait RegisterNumberGenerator {
    def genRegisterNumber: Gen[RegisterNumber] =
      for {
        rnValue <- Gen.chooseNum(minRegisterNumber.value,
          maxRegisterNumber.value)
        rnRefined = refineV[NonNegative](rnValue)
        rnGen <- rnRefined match {
          case Right(rn) => Gen.const(rn)
          case Left(_) => Gen.fail
        }
      } yield rnGen
  }
}
