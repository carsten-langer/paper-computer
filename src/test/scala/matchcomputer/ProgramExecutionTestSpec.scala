package matchcomputer

import eu.timepit.refined.numeric.Positive
import org.scalacheck.Gen
import org.scalatest.EitherValues.{
  convertLeftProjectionToValuable,
  convertRightProjectionToValuable
}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import eu.timepit.refined.refineV

class ProgramExecutionTestSpec
    extends FlatSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers {
  /*
      // todo löschen?
    trait Fixture extends CommonFixtures.RegisterNumberGenerator {

          def genRegisters: Gen[Registers] =
        for {
          minRV <- Arbitrary.arbitrary[RegisterValue]
          maxRV <- Gen.chooseNum(minRV, maxRegisterValue)
          genRV = Gen.chooseNum(minRV, maxRV)
          genRnRv = Gen.zip(genRegisterNumber, genRV)
          rnRvs <- Gen.mapOf(genRnRv)
          mors = Registers(minRV, maxRV, rnRvs)
          rs <- mors match {
            case Right(rs) => Gen.const(rs)
            case Left(_)   => Gen.fail
          }
        } yield rs
    }
   */
  trait NextLineNumberFixture {
    def value2LineNumberGen(v: Value): Gen[LineNumber] =
      refineV[Positive](v) match {
        case Right(ln) => Gen.const(ln)
        case Left(_)   => Gen.fail
      }

    def linesSetGen(createEmptyList: Boolean,
                    min: => Value,
                    max: => Value): Gen[Set[LineNumber]] =
      if (createEmptyList) Gen.const(Set.empty)
      else
        for {
          lnV <- Gen.chooseNum(min, max)
          lns <- Gen.containerOf[Set, LineNumber](value2LineNumberGen(lnV))
        } yield lns

    val linesMinToCurrentLineGen: Gen[(LineNumbers, LineNumber)] = for {
      currentLineV: Value <- Gen.posNum[Value]
      currentLine: LineNumber <- value2LineNumberGen(currentLineV)
      lnsBefore: LineNumbers <- linesSetGen(currentLine == minLineNumber,
                                            minLineNumber.value,
                                            currentLineV - 1)
      lines: LineNumbers = lnsBefore + currentLine
    } yield (lines, currentLine)

    val linesCurrentNextGen: Gen[(LineNumbers, LineNumber, LineNumber)] = for {
      (linesToCurrentLine, currentLine) <- linesMinToCurrentLineGen.suchThat(
        _._2.value < maxLineNumber.value)
      nextLineV: Value <- Gen.chooseNum(currentLine.value + 1,
                                        maxLineNumber.value)
      nextLine: LineNumber <- value2LineNumberGen(nextLineV)
      lnsAfter: LineNumbers <- linesSetGen(nextLine == maxLineNumber,
                                           nextLineV + 1,
                                           maxLineNumber.value)
      lines: LineNumbers = linesToCurrentLine + nextLine ++ lnsAfter
    } yield (lines, currentLine, nextLine)
  }

  behavior of "ProgramExecution.startLineNumber"

  it should "return None for empty program lines" in {
    val lnO = ProgramExecution.startLineNumber(Set.empty)
    lnO.shouldEqual(None)
  }

  it should "return smallest number from program lines" in new CommonFixtures.LineLumberGenerator
  with NextLineNumberFixture {
    val linesGen: Gen[(LineNumbers, LineNumber)] = for {
      startLine <- lineGen
      otherLines <- linesSetGen(startLine == maxLineNumber,
                                startLine.value + 1,
                                maxLineNumber.value)
      lines = otherLines + startLine
    } yield (lines, startLine)

    forAll((linesGen, "lines, start line")) {
      case (lns: LineNumbers, startLine: LineNumber) =>
        val lnO = ProgramExecution.startLineNumber(lns)
        lnO.shouldEqual(Some(startLine))
    }
  }

  behavior of "ProgramExecution.nextLineNumberFromCurrentLine"

  it should "fail if program lines are empty or only have lines less than current line" in new CommonFixtures.LineLumberGenerator
  with NextLineNumberFixture {
    val linesGen: Gen[(LineNumbers, LineNumber)] = for {
      currentLine <- lineGen
      otherLines <- linesSetGen(currentLine == minLineNumber,
                                minLineNumber.value,
                                currentLine.value - 1)
      nonEmptylines = otherLines + currentLine
      lines <- Gen.frequency((10, nonEmptylines),
                             (1, Gen.const(Set.empty[LineNumber])))
    } yield (lines, currentLine)

    forAll((linesGen, "lines, current line")) {
      case (lns: LineNumbers, currentLine: LineNumber) =>
        val morln =
          ProgramExecution.nextLineNumberFromCurrentLine(lns, currentLine)
        morln.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
    }
  }

  it should "return next number" in new CommonFixtures.LineLumberGenerator
  with NextLineNumberFixture {
    forAll((linesCurrentNextGen, "lines, current line, next line")) {
      case (lns: LineNumbers, currentLine: LineNumber, nextLine: LineNumber) =>
        val morln =
          ProgramExecution.nextLineNumberFromCurrentLine(lns, currentLine)
        morln.right.value.shouldEqual(nextLine)
    }
  }

  behavior of "ProgramExecution.nextLineNumberFromProgramControl for Stop"

  it should "return always None" in new CommonFixtures.LineLumberGenerator {
    val linesGen: Gen[LineNumbers] = Gen.containerOf[Set, LineNumber](lineGen)
    forAll((linesGen, "lineNumbers"), (lineGen, "lineNumber")) {
      (lns: LineNumbers, ln: LineNumber) =>
        val morlno: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(lns, ln, Stop)
        morlno.right.value.shouldEqual(None)
    }
  }

  behavior of "ProgramExecution.nextLineNumberFromProgramControl for ContinueWithNextLine"

  it should "fail for empty set of lines" in new CommonFixtures.LineLumberGenerator {
    val emptyLines = Set.empty[LineNumber]
    forAll((lineGen, "lineNumber")) { ln: LineNumber =>
      val morlno: Mor[Option[LineNumber]] =
        ProgramExecution.nextLineNumberFromProgramControl(emptyLines,
                                                          ln,
                                                          ContinueWithNextLine)
      morlno.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
    }
  }

  it should "fail for last line number" in new NextLineNumberFixture {
    forAll((linesMinToCurrentLineGen, "lineNumbers, current line")) {
      case (lns: LineNumbers, cln: LineNumber) =>
        val morlo: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(
            lns,
            cln,
            ContinueWithNextLine)
        morlo.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
    }
  }

  it should "return correct next line number" in new NextLineNumberFixture {
    forAll((linesCurrentNextGen, "lineNumbers, current line, next line")) {
      case (lns: LineNumbers, cln: LineNumber, nln: LineNumber) =>
        val morlno: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(
            lns,
            cln,
            ContinueWithNextLine)
        morlno.right.value.shouldEqual(Some(nln))
    }
  }

  behavior of "ProgramExecution.nextLineNumberFromProgramControl for ContinueWithSecondNextLine"

  it should "fail for empty set of lines" in new CommonFixtures.LineLumberGenerator {
    val emptyLines = Set.empty[LineNumber]
    forAll((lineGen, "lineNumber")) { ln: LineNumber =>
      val morlno: Mor[Option[LineNumber]] =
        ProgramExecution.nextLineNumberFromProgramControl(
          emptyLines,
          ln,
          ContinueWithSecondNextLine)
      morlno.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
    }
  }

  it should "fail for last line number" in new NextLineNumberFixture {
    forAll((linesMinToCurrentLineGen, "lineNumbers, current line")) {
      case (lns: LineNumbers, cln: LineNumber) =>
        val morlo: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(
            lns,
            cln,
            ContinueWithSecondNextLine)
        morlo.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
    }
  }

  it should "fail for second last line number" in new NextLineNumberFixture {
    val linesGen: Gen[(LineNumbers, LineNumber)] = for {
      (linesToCurrentLine, currentLine) <- linesMinToCurrentLineGen.suchThat(
        _._2.value < maxLineNumber.value)
      nextLineV: Value <- Gen.chooseNum(currentLine.value + 1,
                                        maxLineNumber.value)
      nextLine: LineNumber <- value2LineNumberGen(nextLineV)
      lines: LineNumbers = linesToCurrentLine + nextLine
    } yield (lines, currentLine)

    forAll((linesGen, "lineNumbers, current line")) {
      case (lns: LineNumbers, cln: LineNumber) =>
        val morlo: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(
            lns,
            cln,
            ContinueWithSecondNextLine)
        morlo.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
    }
  }

  it should "return correct next line number" in new NextLineNumberFixture {
    val linesGen: Gen[(LineNumbers, LineNumber, LineNumber)] = for {
      (linesToCurrentLine, currentLine) <- linesMinToCurrentLineGen.suchThat(
        _._2.value < maxLineNumber.value - 1)
      nextLineV: Value <- Gen.chooseNum(currentLine.value + 1,
                                        maxLineNumber.value - 1)
      nextLine: LineNumber <- value2LineNumberGen(nextLineV)
      secondNextLineV: Value <- Gen.chooseNum(nextLineV + 1,
                                              maxLineNumber.value)
      secondNextLine: LineNumber <- value2LineNumberGen(secondNextLineV)
      lnsAfter: LineNumbers <- linesSetGen(secondNextLine == maxLineNumber,
                                           secondNextLineV + 1,
                                           maxLineNumber.value)
      lines: LineNumbers = linesToCurrentLine + nextLine + secondNextLine ++ lnsAfter
    } yield (lines, currentLine, secondNextLine)

    forAll((linesGen, "lineNumbers, current line, second next line")) {
      case (lns: LineNumbers, cln: LineNumber, snln: LineNumber) =>
        val morlno: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(
            lns,
            cln,
            ContinueWithSecondNextLine)
        morlno.right.value.shouldEqual(Some(snln))
    }
  }

  behavior of "ProgramExecution.nextLineNumberFromProgramControl for ContinueWithLine"

  it should "return always Some(jmp line number)" in new CommonFixtures.LineLumberGenerator {
    val linesGen: Gen[LineNumbers] = Gen.containerOf[Set, LineNumber](lineGen)
    forAll((linesGen, "lineNumbers"),
           (lineGen, "current lineNumber"),
           (lineGen, "jmp lineNumber")) {
      (lns: LineNumbers, cln: LineNumber, jmpLn: LineNumber) =>
        val morlno: Mor[Option[LineNumber]] =
          ProgramExecution.nextLineNumberFromProgramControl(
            lns,
            cln,
            ContinueWithLine(jmpLn))
        morlno.right.value.shouldEqual(Some(jmpLn))
    }
  }

  /*
    it should "fail for empty set of lines" in new CommonFixtures.LineLumberGenerator {
      val emptyLines = Set.empty[LineNumber]
      forAll((lineGen, "current lineNumber"), (lineGen, "jump lineNumber")) {
        (cln: LineNumber, jmpLn: LineNumber) =>
          val morlno: Mor[Option[LineNumber]] =
            ProgramExecution.nextLineNumberFromProgramControl(
              emptyLines,
              cln,
              ContinueWithLine(jmpLn))
          morlno.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
      }
    }

    it should "fail if jump line number is not in non-empty set of lines" in new CommonFixtures.LineLumberGenerator {
      val linesGen: Gen[(LineNumbers, LineNumber)] = for {
        min2lines: LineNumbers <- Gen
          .containerOf[Set, LineNumber](lineGen)
          .suchThat(_.size > 1)
        linesToRemove: Seq[LineNumber] <- Gen.pick(1, min2lines)
        lineToRemove: LineNumber = linesToRemove.head
      } yield (min2lines, lineToRemove)

      forAll((linesGen, "min 2 lines, line to remove"),
             (lineGen, "current lineNumber")) {
        (linesLineToRemoveTuple: (LineNumbers, LineNumber), cln: LineNumber) =>
          linesLineToRemoveTuple match {
            case (min2lines, lineToRemove) =>
              val lns = min2lines - lineToRemove
              val morlno: Mor[Option[LineNumber]] =
                ProgramExecution.nextLineNumberFromProgramControl(
                  lns,
                  cln,
                  ContinueWithLine(lineToRemove))
              morlno.left.value.shouldEqual(NoNextLinenumberFoundInProgram)
          }
      }
    }
 */
}
