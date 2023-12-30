package myzio

import scala.collection.immutable.LazyList.cons
import zio.http.Decompression.No

object google:
  type Google = Has[Google.Service]
  object Google:
    trait Service:
      def count(topic: String): ZIO[Any, Nothing, Int]
    lazy val live: ZIO[Any, Nothing, Google.Service] =
      ZIO.fromFunction(_ => make)
    lazy val make: Google.Service = new:
      override def count(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cat" then 1 else 2)

object bizLogic:
  type BizLogic = Has[BizLogic.Service]
  object BizLogic:
    trait Service:
      def isGoogleResultEven(topic: String): ZIO[Any, Nothing, Boolean]
    lazy val live: ZIO[google.Google.Service, Nothing, BizLogic.Service] =
      ZIO.fromFunction(make)
    def make(g: google.Google.Service): Service = new:
      override def isGoogleResultEven(
          topic: String
      ): ZIO[Any, Nothing, Boolean] =
        g.count(topic).map(_ % 2 == 0)

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, bizLogic.BizLogic.Service] =
    for {
      g <- google.Google.live
      bl <- bizLogic.BizLogic.live.provide(g)
    } yield bl
  lazy val make: bizLogic.BizLogic.Service =
    bizLogic.BizLogic.make(google.Google.make)

object MainZioInjection extends scala.App:
  Runtime.default.unsafeRunSync(
    for
      g <- google.Google.live
      bl <- bizLogic.BizLogic.live.provide(g)
      p <- program.provideSome[ZENV](Has(bl) union _)
    yield p
  )

  lazy val program =
    for
      env <- ZIO.identity[console.Console & bizLogic.BizLogic]
      _ <- env.get[console.Console.Service].printLine("-" * 50)
      cat <- env.get[bizLogic.BizLogic.Service].isGoogleResultEven("cat")
      _ <- env.get[console.Console.Service].printLine(cat.toString)
      dog <- env.get[bizLogic.BizLogic.Service].isGoogleResultEven("dog")
      _ <- env.get[console.Console.Service].printLine(dog.toString)
      _ <- env.get[console.Console.Service].printLine("-" * 50)
    yield ()
