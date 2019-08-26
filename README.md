# paper-computer
A functional Scala library implementation of a computer working with a sheet of paper, a pen and a set of items e.g. matches
(Streichholzcomputer / Papiercomputer), used for education in the 1980s.

For background see
* [WDR paper computer](https://en.wikipedia.org/wiki/WDR_paper_computer)
* [Know-how-Computer](https://de.wikipedia.org/wiki/Know-how-Computer) (German)

## Architecture
A paper-computer is a state-machine. Each state consists of a set of registers, holding the register value
information, and the information which line in the program would be executed next. 

The execution of a single program line will either fail (e.g. trying to increment a non-existing register, or jumping to a
non-existing next program line) or return a new state with potentially changed register values and a changed
command line to execute next. 

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
We have:
```scala
type NonNegativeValue = Value Refined NonNegative
type RegisterNumber = NonNegativeValue
type RegisterValue = Value
type RegisterValues = immutable.Map[RegisterNumber, RegisterValue]
class Registers (minRegisterValue: RegisterValue, maxRegisterValue: RegisterValue, registerValues: RegisterValues)
```
That is: `Registers` holds a map from `RegisterNumber` to `RegisterValue`.
The `minRegisterValue` and `maxRegisterValue` allow to simulate in the paper computer various real-world architectures,
e.g. signed 8-bit registers with a value range from -128 to 127,
or unsigned 16-bit registers with a value range from 0 to 65,535, including wrap-around.
For example, incrementing a signed 8-bit register with value 127 results in a new value of -128, like in real world.

The creation of `Registers` is implemented such that no illegal state can be represented.  

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
register or jumping to a non existing program line. For convenience, these `Either` types have type aliases, e.g.:

```scala
type Mor[T] = Either[Message, T]
type MorRegisterValue = Mor[RegisterValue]
type MorRegisters = Mor[Registers]
```

### Program Execution
A program execution takes a program and an initial registers state and returns either the resulting registers state
at the end of the program, or an error message, e.g. `IllegalAccessToNonExistingRegisterNumber`.

```scala
type ProgramExecution = (Program, Registers) => MorRegisters
```

By convention the execution of a program starts at its lowest line number. Empty programs can be created,
but cannot be executed.

Execution is fully functional using tailrec so that no stack overflow occurs.

### Command Enhancements
This implementation of the paper-computer adds 2 commands for convenience:

| Command | Meaning |
| --- | --- |
| `Prg(program)` | Execute program _program_, starting with that program's lowest line number, and an initial registers state of this program's current registers state. If that program reaches its end via `Stp`, continue with the next line in this program using that program's  resulting registers state. While technically not needed to write programs, this addition allows for easier implementation of a library of pre-defined programs that can be plugged together. |
| `Sub(ln)` | Jump to line _ln_, but when the execution ends via `Stp`, continue with next line after this `Sub` in the program. While technically not needed to write programs, this simplifies calling a section of this program from different places and continuing from the different respective next lines. | 

## Example Usage
The following program adds up the values in register 2 and register 3 and returns the result in register 1, thereby
destroying the original values in registers 1, 2 and 3.
```scala
import papercomputer._

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
    val morRegisters: MorRegisters = Registers(minRegisterValue = -21, maxRegisterValue = 42L,
          registerValues = Map((1L, 42L), (2L, 4L), (3L, 5L))
        )
    
    val result: MorRegisterValue = for {
        initialRegisters <- morRegisters
        resultingRegisters <- ProgramExecution.execute(programAdditionR2PlusR3ToR1, initialRegisters)
        resultingR1 = resultingRegisters.registerValues(1L)
    } yield resultingR1
    // Right(9)
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
the whole value space in positive direction, wrap around from highest value to lowese value and continue to increment
until the program stops.
 
The end result will be correct, but the number of loop iterations may be
up to the size of your value range minus 1, i.e. for simulating an 8-bit register up to 255 steps, for simulating a
16-bit register already up to 65,536 steps.    
