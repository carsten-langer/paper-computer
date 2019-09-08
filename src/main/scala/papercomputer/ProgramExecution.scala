package papercomputer

import cats.data.Kleisli
import cats.effect.IO
import cats.implicits.toFunctorOps
import cats.{ApplicativeError, Functor}
import fs2.{Pure, Stream}

object ProgramExecution {
  implicit val registersFactory: Kleisli[Mor, RegistersConfig, Registers] =
    Registers.fromRegistersConfig

  def liftMap[F[_], T](purePsStream: Stream[Pure, Mor[ProgramState]],
                       f: Mor[ProgramState] => F[Mor[T]]): Stream[F, Mor[T]] =
    purePsStream.flatMap(f.andThen(Stream.eval))

  def last[F[_]: Functor, T](nonEmptyStream: Stream[F, Mor[T]])(
      implicit compiler: Stream.Compiler[F, F]): F[Mor[T]] =
    nonEmptyStream.lastOr(Left(IllegalState)).compile.toList.map(_.head)

  def execute[F[_], T](stream: Stream[Pure, Mor[ProgramState]],
                       observationF: Mor[ProgramState] => F[Mor[T]])(
      implicit ae: ApplicativeError[F, Throwable],
      compiler: Stream.Compiler[F, F]): F[Mor[T]] =
    last(liftMap(stream, observationF))

  def execute[F[_], T](program: Program,
                       registersConfig: RegistersConfig,
                       observationF: Mor[ProgramState] => F[Mor[T]])(
      implicit ae: ApplicativeError[F, Throwable],
      compiler: Stream.Compiler[F, F]): F[Mor[T]] = {
    val programStateConfig = ProgramStateConfig(program, registersConfig)
    val stream: Stream[Pure, Mor[ProgramState]] =
      ProgramState.streamFromProgramStateConfig(programStateConfig)
    execute[F, T](stream, observationF)
  }

  val execute: ProgramExecutionF =
    (program: Program, registersConfig: RegistersConfig) =>
      execute[IO, Registers](
        program,
        registersConfig,
        (morPs: Mor[ProgramState]) => IO(morPs.map(_.registers)))
        .unsafeRunSync()
}
