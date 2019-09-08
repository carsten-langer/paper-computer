package papercomputer

import cats.data.StateT
import cats.effect.IO
import cats.implicits.catsStdInstancesForEither
import fs2.{Pure, Stream}
import org.scalacheck.Gen
import org.scalatest.EitherValues.convertRightProjectionToValuable
import org.scalatest.{Assertions, FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ProgramExecutionTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  def _PRINTOUT_DURING_TESTS_ = false

  trait Fixture extends CommonFixtures {
    lazy val nextShouldNotBeCalled: StateT[Mor, ProgramState, ProgramState] =
      StateT[Mor, ProgramState, ProgramState](_ =>
        Assertions.fail(" next should not be called"))

    val genRegistersConfig: Gen[(RegistersConfig, Registers)] = genRegisters
      .map(
        registers =>
          (RegistersConfig(registers.minRegisterValue,
                           registers.maxRegisterValue,
                           registers.registerValues),
           registers))
  }

  behavior of "ProgramExecution.execute"

  it should "call the observationF with right program states in right order and return its last result" in new Fixture {
    val gen: Gen[(Stream[Pure, Mor[ProgramState]], List[Registers])] = for {
      stream <- genNonEmptyStream
      size = stream.toList.size
      registersList <- Gen.listOfN(size, genRegisters)
    } yield (stream, registersList)

    forAll(gen) {
      case (stream, registersList) =>
        whenever(stream.toList.nonEmpty && registersList.nonEmpty) {
          val pss = stream.toList
          var i: Int = -1
          def observationF: Mor[ProgramState] => IO[Mor[Registers]] =
            morPs =>
              IO {
                i += 1
                val _ = morPs.shouldEqual(pss(i))
                if (_PRINTOUT_DURING_TESTS_) println(morPs)
                Right(registersList(i))
            }
          val ioMorRegisters: IO[Mor[Registers]] = ProgramExecution
            .execute(stream, observationF)
          val morRegisters: Mor[Registers] = ioMorRegisters
            .unsafeRunSync()
          i.shouldEqual(registersList.size - 1)
          morRegisters.right.value.shouldEqual(registersList.last)
        }
    }
  }

}
