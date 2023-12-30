package myzio

import scala.reflect.ClassTag

case class ZIO[-R, +E, +A](run: R => Either[E, A]):
  def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
    for
      a <- this
      b <- that
    yield (a, b)

  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO { r =>
      run(r).fold(ZIO.fail, azb).run(r)
    }

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO { r => run(r).map(ab) }

  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO { r =>
      run(r).fold(h, ZIO.succeed).run(r)
    }
  def mapError[E2](h: E => E2): ZIO[R, E2, A] =
    ZIO { r => run(r).left.map(h) }

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))

  def provideSome[R0](f: R0 => R): ZIO[R0, E, A] =
    ZIO.accessM(r0 => provide(f(r0)))

object ZIO:
  def succeed[A](a: => A): ZIO[Any, Nothing, A] = ZIO(_ => Right(a))
  def fail[E](e: => E): ZIO[Any, E, Nothing] = ZIO(_ => Left(e))
  def effect[A](a: => A): ZIO[Any, Throwable, A] = ZIO(_ =>
    try Right(a)
    catch Left(_)
  )
  def fromFunction[R, A](run: R => A): ZIO[R, Nothing, A] =
    ZIO(r => Right(run(r)))

  def access[R]: AccessPartiallyApplied[R] = AccessPartiallyApplied()

  def accessM[R]: AccessMPartiallyApplied[R] = AccessMPartiallyApplied()

  def identity[R]: ZIO[R, Nothing, R] =
    fromFunction(Predef.identity)

final class AccessMPartiallyApplied[R]:
  def apply[E, A](f: R => ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.identity.flatMap(f)

final class AccessPartiallyApplied[R]:
  def apply[A](f: R => A): ZIO[R, Nothing, A] =
    ZIO.identity.map(f)

object console:
  type Console = Has[Console.Service]
  object Console:
    trait Service:
      def printLine(line: => String): ZIO[Any, Nothing, Unit]
      def getLine: ZIO[Any, Nothing, String]
    lazy val live: ZLayer[Any, Nothing, Console] = ZLayer.succeed(make)
    lazy val make: Service =
      new:
        override def getLine: ZIO[Any, Nothing, String] =
          ZIO.succeed(scala.io.StdIn.readLine())
        override def printLine(line: => String): ZIO[Any, Nothing, Unit] =
          ZIO.succeed(println(line))

object Runtime:
  object default:
    def unsafeRunSync[E, A](zio: => ZIO[ZENV, E, A]): Either[E, A] =
      zio.provide(Has(console.Console.make)).run(())

type ZENV = console.Console

final class Has[A] private (private val map: Map[String, Any])

object Has:
  def apply[A](a: A)(using tag: ClassTag[A]): Has[A] =
    new Has(Map(tag.toString -> a))
  extension [A <: Has[?]](a: A)
    inline def ++[B <: Has[?]](b: B): A & B =
      a.union(b)

    infix def union[B <: Has[?]](b: B): A & B =
      new Has(a.map ++ b.map).asInstanceOf[A & B]

    def get[S](using tag: ClassTag[S])(using A => Has[S]): S =
      a.map(tag.toString()).asInstanceOf[S]

final class ZLayer[-R, +E, +A](val zio: ZIO[R, E, A])
object ZLayer:
  def succeed[A: ClassTag](a: => A): ZLayer[Any, Nothing, Has[A]] =
    ZLayer(ZIO.succeed(Has(a)))
  def fromService[R <: Has[S], S: ClassTag, A: ClassTag](
      f: S => A
  ): ZLayer[R, Nothing, Has[A]] =
    ZLayer(ZIO.fromFunction(r => Has(f(r.get))))
  def fromServices[
      R <: Has[S1] & Has[S2],
      S1: ClassTag,
      S2: ClassTag,
      A: ClassTag
  ](
      f: (S1, S2) => A
  ): ZLayer[R, Nothing, Has[A]] =
    ZLayer(ZIO.fromFunction(r => Has(f(r.get[S1], r.get[S2]))))
