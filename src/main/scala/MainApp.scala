import myzio.*

object MainApp extends App:
  println(
    Runtime.default.unsafeRunSync(
      program.provide(console.Console.make)
    )
  )

  lazy val program = for
    console_ <- ZIO.identity[console.Console.Service]
    _ <- console_.printLine("-" * 20)
    _ <- console_.printLine("what's your name?")
    name <- ZIO.succeed("Wenshuai")
    _ <- ZIO
      .effect(throw RuntimeException("boom"))
      .catchAll(x => ZIO.succeed(println(x)))
    _ <- console_.printLine(s"hello, $name")
    _ <- console_.printLine("-" * 20)
  yield ()
