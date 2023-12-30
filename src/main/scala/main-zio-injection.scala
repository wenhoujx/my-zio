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

object controller:
  type Controller = Has[Controller.Service]
  object Controller:
    trait Service:
      def run: ZIO[Any, Nothing, Unit]
    lazy val live: ZIO[bizLogic.BizLogic & console.Console, Nothing, Service] =
      ZIO.fromFunction { env =>
        val bl = env.get[bizLogic.BizLogic.Service]
        val con = env.get[console.Console.Service]
        make(bl, con)
      }
    def make(
        bl: bizLogic.BizLogic.Service,
        con: console.Console.Service
    ): Service =
      new:
        override def run: ZIO[Any, Nothing, Unit] =
          for
            _ <- con.printLine("-" * 50)
            cat <- bl.isGoogleResultEven("cat")
            _ <- con.printLine(cat.toString)
            dog <- bl.isGoogleResultEven("dog")
            _ <- con.printLine(dog.toString)
            _ <- con.printLine("-" * 50)
          yield ()

object MainZioInjection extends scala.App:
  Runtime.default.unsafeRunSync(
    program.flatMap(_.run)
  )

  lazy val program =
    for
      g <- google.Google.live
      bl <- bizLogic.BizLogic.live.provide(g)
      con <- console.Console.live
      c <- controller.Controller.live.provide(Has(bl) ++ Has(con))
    yield c
