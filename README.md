[![Build Status](https://travis-ci.org/carsten-langer/paper-computer.svg?branch=master)](https://travis-ci.org/carsten-langer/paper-computer)

# paper-computer
A _functional_ Scala library implementation of a computer working with a sheet of paper, a pen,
and a set of items e.g. matches (Streichholzcomputer / Papiercomputer), used for education in the 1980s.

For background see
* [WDR paper computer](https://en.wikipedia.org/wiki/WDR_paper_computer)
* [Know-how-Computer](https://de.wikipedia.org/wiki/Know-how-Computer) (German)

## Architecture
A paper-computer is a state-machine. Each state consists of a set of registers, holding the register value
information, and the information which line in the program would be executed next. 

The execution of a single program line will either fail (e.g. trying to increment a non-existing register,
or jumping to a non-existing next program line) or return a new state with potentially changed register values,
and a changed command line to execute next. 

The execution of a whole program will start execution at the first command and move the state machine forward until
a `Stp` command is encountered.

### Value ranges
The simulated architecture of the paper-computer builds on top of a `Value` type, which can be chosen to be Scala's
`Long` type or e.g. `Int` to reduce resource usage:

```scala
type Value = Long
val minValue: Value = Long.MinValue
val maxValue: Value = Long.MaxValue
```

###  Registers
There is:
```scala
type NonNegativeValue = Value Refined NonNegative
type RegisterNumber = NonNegativeValue
type RegisterValue = Value
type RegisterValues = immutable.Map[RegisterNumber, RegisterValue]
case class RegistersConfig(minRegisterValue: RegisterValue, maxRegisterValue: RegisterValue, registerValues: RegisterValues)
case class Registers(minRegisterValue: RegisterValue, maxRegisterValue: RegisterValue, registerValues: RegisterValues)
```
That is: `Registers` holds a map from `RegisterNumber` to `RegisterValue`.
The `minRegisterValue` and `maxRegisterValue` allow to simulate in the paper computer various real-world architectures,
e.g. signed 8-bit registers with a value range from -128 to 127,
or unsigned 16-bit registers with a value range from 0 to 65,535, including wrap-around.
For example, incrementing a signed 8-bit register with value 127 results in a new value of -128, like in real world.

A `Registers` is actually created from a `RegistersConfig`, which has the same structure. However, `Registers` creation
guarantees that no illegal state can be represented, while `RegistersConfig` is unchecked.  

### Original Commands
The original paper-computer knows 5 commands:

| Command | Meaning |
| --- | --- |
| `Inc(rn)` | Increment the register with register number _rn_, then continue with the next line in the program. |
| `Dec(rn)` | Decrement the register with register number _rn_, then continue with the next line in the program. |
| `Isz(rn)` | If the register with register number _rn_ has value zero, continue with the second next line in the program, else continue with the next line in the program. |
| `Jmp(ln)` | Jump, i.e. continue with line _ln_ in the program. |
| `Stp` | Stop executing this program. |

### Program
A program is simply a map from line number to command:

```scala
type Program = immutable.Map[LineNumber, Command]
```

### Error Handling
Throughout this library errors are represented by type `Message`. Potentially failing functions return an 
`Either[Message, T]` with `T` the successful return type. Examples for failure are trying to access a non existing
register or jumping to a non existing program line. The whole library basically work in the effect of `Mor[_]`:

```scala
sealed trait Message
case object MinRegisterValueMustBeLessOrEqualZero extends Message
...
```

and 

```scala
type Mor[T] = Either[Message, T]
```

### Program Execution

#### Default execution
A default program execution takes a program and an initial registers state,
and returns either the resulting registers state at the end of the program,
or an error `Message`, e.g. `IllegalAccessToNonExistingRegisterNumber`.

```scala
class RegistersConfig(minRegisterValue: RegisterValue, maxRegisterValue: RegisterValue, registerValues: RegisterValues)
type ProgramExecutionF = (Program, RegistersConfig) => Mor[Registers]
```
In detail:

* Registers are build from a registers configuration.
* Building the registers will fail with a message for an illegal registers configuration.
* By convention the execution of a program starts at its lowest line number.
* Empty programs can be created, but cannot be executed.
* Executing the program may fail with a message.
* Program execution may be infinite if the program contains a loop.
* If no failure occurs and the program finishes, the final registers are returned.

Execution is safe from stack overflows but may not end if you have an unconditional loop in your paper-computer program.

#### Execution stream
Under the hood, execution builds a `Stream` of `ProgramState`s.
Execution can also be effectful and yielding any other result which can be derived from `ProgramState`.
See the example usage below for an example of interleaving an `IO` effect for printing out each intermediate step.
For more details on hooking into the `Stream` see the source code of `ProgramState` and `ProgramExecution`. 

### Command Enhancements
This implementation of the paper-computer adds 2 commands for convenience:

| Command | Meaning |
| --- | --- |
| `Prg(program)` | Execute program _program_, starting with that program's lowest line number, and an initial registers state of this program's current registers state. If that program reaches its end via `Stp`, continue with the next line in this program using that program's  resulting registers state. While technically not needed to write programs, this addition allows for easier implementation of a library of pre-defined programs that can be plugged together, e.g. see the fibonacci calculation in the [Program Library](https://github.com/carsten-langer/paper-computer/blob/master/src/main/scala/papercomputer/ProgramLibrary.scala). |
| `Sub(ln)` | Jump to line _ln_, but when the execution ends via `Stp`, continue with next line after this `Sub` in the program. While technically not needed to write programs, this simplifies calling a section of this program from different places and continuing from the different respective next lines. | 

## Example Usage
The following program adds up the values in register 2 and register 3 and returns the result in register 1, thereby
destroying the original values in registers 1, 2 and 3.
```scala
object demo {
  val programAdditionR2PlusR3ToR1: Program = Map(
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

  // use arbitrary min/max values just to show we can!
  val registersConfig: RegistersConfig = RegistersConfig(minRegisterValue = -21, maxRegisterValue = 42L,
    registerValues = Map((1L, 42L), (2L, 4L), (3L, 5L)))

  val result: Mor[RegisterValue] = for {
    resultingRegisters <- ProgramExecution.execute(programAdditionR2PlusR3ToR1, registersConfig)
    resultingR1 = resultingRegisters.registerValues(1L)
  } yield resultingR1
  // yields: Right(9)

  val liftAndMap: Mor[ProgramState] => IO[Mor[Registers]] = morPs => IO {
    println(morPs)
    morPs.map(_.registers)
  }

  val resultWithObservation: IO[Mor[Registers]] =
    ProgramExecution.execute(programAdditionR2PlusR3ToR1, registersConfig, liftAndMap)
  resultWithObservation.unsafeRunSync()
  // yields: Right(Registers(-21,42,Map(1 -> 9, 2 -> 0, 3 -> 0)))
  // prints out as IO side-effect all intermediate program states:
  // Right(ProgramState(RegistersOpsConfig(papercomputer.RegistersOps$<function>apercomputer.RegistersOps$$$Lambda$7644/154046850@23f67fd9,papercomputer.RegistersOps$$$Lambda$7645/525407211@21d9452c),Map(170 -> Jmp(120), 120 -> Isz(3), 10 -> Isz(1), 110 -> Jmp(60), 20 -> Jmp(40), 60 -> Isz(2), 160 -> Inc(1), 70 -> Jmp(90), 140 -> Stp, 130 -> Jmp(150), 80 -> Jmp(120), 150 -> Dec(3), 50 -> Jmp(10), 40 -> Dec(1), 30 -> Jmp(60), 90 -> Dec(2), 100 -> Inc(1)),List(),Some(10),Registers(-21,42,Map(1 -> 42, 2 -> 4, 3 -> 5))))
  // ...
  //Right(ProgramState(RegistersOpsConfig(papercomputer.RegistersOps$<function>apercomputer.RegistersOps$$$Lambda$7644/154046850@23f67fd9,papercomputer.RegistersOps$$$Lambda$7645/525407211@21d9452c),Map(170 -> Jmp(120), 120 -> Isz(3), 10 -> Isz(1), 110 -> Jmp(60), 20 -> Jmp(40), 60 -> Isz(2), 160 -> Inc(1), 70 -> Jmp(90), 140 -> Stp, 130 -> Jmp(150), 80 -> Jmp(120), 150 -> Dec(3), 50 -> Jmp(10), 40 -> Dec(1), 30 -> Jmp(60), 90 -> Dec(2), 100 -> Inc(1)),List(),None,Registers(-21,42,Map(1 -> 9, 2 -> 0, 3 -> 0))))
}
```

### More Examples
See [ProgramLibrary](https://github.com/carsten-langer/paper-computer/blob/master/src/main/scala/papercomputer/ProgramLibrary.scala) and 
[ProgramLibraryTestSpec](https://github.com/carsten-langer/paper-computer/blob/master/src/test/scala/papercomputer/ProgramLibraryTestSpec.scala).

## Caveats
If you allow negative values in the registers, it will work. However, the original set of 5 commands does not allow
finding out if a given register value is positive or negative, the only test is `Isz`, which tests if a value is zero
or not.

The programs presented here and contained in the library are optimized for positive numbers. For example,
adding 2 registers could be implemented such that as long as the second register is not zero, the first register
is incremented and the second register is decremented. This _while-repeat-loop_ will finally have in the first register
the sum of both original register values.

However, if the second register value is negative, this _while-repeat-loop_ will decrement a negative value to an
even more negative value farther away from zero. It may thus take an awful lot of time to decrement the second value
through the whole value space to reach its lowest value, so that it can then wrap-around to the highest value and
after further decrementing finally reach the zero value. At the same time the first register value will move through
the whole value space in positive direction, wrap around from highest value to lowest value and continue to increment
until the program stops.
 
The end result will be correct, but the number of loop iterations may be
up to the size of your value range minus 1, i.e. for simulating an 8-bit register up to 255 steps, for simulating a
16-bit register already up to 65,536 steps.    
