package papercomputer

import eu.timepit.refined.auto._

/**
  * Library of programs. These are tested in ProgramExecutionTestSpec.
  *
  * Contains explicit programs only using the original 5 commands.
  */
object ProgramLibrary {

  /** Register 1 = 0
    * Using explicit register numbers. For parameterizable register numbers see zeroRx.
    * Make register 1 have the value 0.
    * Assumes that the original value in the is >= 0.
    * If not, the algorithm may be slow as it wraps around in value space. */
  val programZeroR1: Program = Map(
    (10L, Isz(1L)),
    (20L, Jmp(40L)),
    (30L, Stp),
    (40L, Dec(1L)),
    (50L, Jmp(10L))
  )

  /** rn1 = rn1 + rn2
    * Using explicit register numbers. For parameterizable register numbers see rnAPlusRnBToRnA.
    * Adds up registers rn1 and rn1, result in register rn1.
    * For negative value in rn2 this may be slow as the algorithm has to wrap around the value space. */
  val programAdditionR1plusR2toR1: Program = Map(
    (10L, Isz(2L)),
    (20L, Jmp(40L)),
    (30L, Stp),
    (40L, Inc(1L)),
    (50L, Dec(2L)),
    (60L, Jmp(10L))
  )

  /** rn1 = rn2 + rn3
    * Using explicit register numbers. For parameterizable register numbers see rbPlusRcToRa.
    * Adds up registers rn2 and rn3, result in register rn1.
    * Assumes values in rn2 and rn3 both being non-negative,
    * else performance may be slow due to the need to wrap-around in the value space. */
  val programAdditionR2plusR3toR1: Program = Map(
    (10L, Isz(1L)),
    (20L, Jmp(40L)),
    (30L, Jmp(60L)),
    (40L, Dec(1L)),
    (50L, Jmp(10L)),
    (60L, Isz(2L)),
    (70L, Jmp(90L)),
    (80L, Jmp(120L)),
    (90L, Dec(2L)),
    (100L, Inc(1L)),
    (110L, Jmp(60L)),
    (120L, Isz(3L)),
    (130L, Jmp(150L)),
    (140L, Stp),
    (150L, Dec(3L)),
    (160L, Inc(1L)),
    (170L, Jmp(120L))
  )

  /** rnToZero = 0
    * Make register rnToZero have the value 0.
    * Assumes that the original value in the is >= 0.
    * If not, the algorithm may be slow as it wraps around in value space. */
  def zeroRx(rnToZero: RegisterNumber): Program = Map(
    (1L, Isz(rnToZero)),
    (2L, Jmp(4L)),
    (3L, Stp),
    (4L, Dec(rnToZero)),
    (5L, Jmp(1L))
  )

  /** rnA = rnA + rnB
    * Adds up registers rnA and rnB, result in register rnA.
    * For negative value in rnB this may be slow as the algorithm has to wrap around the value space. */
  def raPlusRbToRa(rnA: RegisterNumber, rnB: RegisterNumber): Program =
    Map((1L, Isz(rnB)),
        (2L, Jmp(4L)),
        (3L, Stp),
        (4L, Inc(rnA)),
        (5L, Dec(rnB)),
        (6L, Jmp(1L)))

  /** rnA = rnB + rnC
    * Adds up registers rnB and rnC, result in register rnA.
    * Assumes values in rnB and rnC both being non-negative,
    * else performance may be slow due to the need to wrap-around in the value space. */
  def rbPlusRcToRa(rnA: RegisterNumber,
                   rnB: RegisterNumber,
                   rnC: RegisterNumber): Program = Map(
    (1L, Prg(zeroRx(rnA))),
    (2L, Prg(raPlusRbToRa(rnA, rnB))),
    (3L, Prg(raPlusRbToRa(rnA, rnC))),
    (4L, Stp)
  )

  /** toRn = fromRn
    * Copies value in register fromRn to register toRn, leaving the original value in fromRn intact, using
    * one temporary register. */
  def copy(fromRn: RegisterNumber,
           toRn: RegisterNumber,
           tmpRn: RegisterNumber): Program =
    Map(
      (1L, Prg(zeroRx(toRn))),
      (2L, Prg(zeroRx(tmpRn))),
      (3L, Isz(fromRn)),
      (4L, Jmp(6L)),
      (5L, Jmp(10L)), // now fromRn is 0, toRn = tmpRn = original fromRn
      (6L, Dec(fromRn)),
      (7L, Inc(toRn)),
      (8L, Inc(tmpRn)),
      (9L, Jmp(3L)),
      (10L, Prg(raPlusRbToRa(fromRn, tmpRn))), // start copying back tmpRn to fromRn
      (11L, Stp)
    )

  /** rnA = rnB * rnC
    * Multiplies registers rnB and rnC, result in register rnA, using 2 temporary registers. */
  def multiplyRbWithRcToRa(rnA: RegisterNumber,
                           rnB: RegisterNumber,
                           rnC: RegisterNumber,
                           rnTmp1: RegisterNumber,
                           rnTmp2: RegisterNumber): Program =
    Map(
      (1L, Prg(zeroRx(rnA))),
      (2L, Isz(rnC)),
      (3L, Jmp(5L)),
      (4L, Stp),
      (5L, Prg(copy(rnB, rnTmp1, rnTmp2))),
      (6L, Prg(raPlusRbToRa(rnA, rnTmp1))),
      (7L, Dec(rnC)),
      (8L, Jmp(2L))
    )

  /** rnA = rnA - rnB
    * Subtracts register rnB from rnA, result in register rnA. */
  def raMinusRbToRa(rnA: RegisterNumber, rnB: RegisterNumber): Program = Map(
    (1L, Isz(rnB)),
    (2L, Jmp(4L)),
    (3L, Stp),
    (4L, Dec(rnB)),
    (5L, Dec(rnA)),
    (6L, Jmp(1L))
  )

  /** Calculate Fibonacci number for value in rnA.
    * The register number rnA holds the number of the Fibonacci number to calculate, must be >= 0.
    * Example: 8 yields Fibonacci(8).
    * The register number rnB holds the result, e.g. 21.
    */
  def fibonacci(rnA: RegisterNumber,
                rnB: RegisterNumber,
                rnT1: RegisterNumber,
                rnT2: RegisterNumber,
                rnT3: RegisterNumber): Program = Map(
    (10L, Prg(zeroRx(rnB))),
    (20L, Isz(rnA)),
    (30L, Jmp(50L)),
    (40L, Stp),
    (50L, Prg(zeroRx(rnT1))),
    (60L, Inc(rnB)),
    (70L, Dec(rnA)),
    (80L, Isz(rnA)),
    (90L, Jmp(110L)),
    (100L, Stp),
    (110L, Prg(copy(rnT1, rnT2, rnT3))),
    (120L, Prg(copy(rnB, rnT1, rnT3))),
    (130L, Prg(raPlusRbToRa(rnB, rnT2))),
    (140L, Jmp(70L))
  )

}
