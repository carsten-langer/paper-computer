package papercomputer

import eu.timepit.refined.auto._

object ProgramLibrary {

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
  def rnAPlusRnBToRnA(rnA: RegisterNumber, rnB: RegisterNumber): Program =
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
    (2L, Prg(rnAPlusRnBToRnA(rnA, rnB))),
    (3L, Prg(rnAPlusRnBToRnA(rnA, rnC))),
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
      (5L, Jmp(10L)),
      (6L, Dec(fromRn)),
      (7L, Inc(toRn)),
      (8L, Inc(tmpRn)),
      (9L, Jmp(3L)),
      (10L, Isz(tmpRn)),
      (11L, Jmp(13L)),
      (12L, Stp),
      (13L, Dec(tmpRn)),
      (14L, Inc(fromRn)),
      (15L, Jmp(10L))
    )

  /** rnA = rnB * rnC
    * Multiplies registers rnB and rnC, result in register rnA, using 3 temporary registers. */
  def multiplyRnBWithRnCToRnA(rnA: RegisterNumber,
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
      (6L, Prg(rnAPlusRnBToRnA(rnA, rnTmp1))),
      (7L, Dec(rnC)),
      (8L, Jmp(2L))
    )

  /** rnA = rnA - rnB
    * Substracts register rnB from rnA, result in register rnA. */
  def rnAMinusRnBToRnA(rnA: RegisterNumber, rnB: RegisterNumber): Program = Map(
    (1L, Isz(rnB)),
    (2L, Jmp(4L)),
    (3L, Stp),
    (4L, Dec(rnB)),
    (5L, Dec(rnA)),
    (6L, Jmp(1L))
  )

}
