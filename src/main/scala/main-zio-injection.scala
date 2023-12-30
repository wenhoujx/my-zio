package myzio

import scala.collection.immutable.LazyList.cons
import zio.http.Decompression.No

object google:
  type Google = Has[Google.Service]
  object Google:
    trait Service:
      def count(topic: String): ZIO[Any, Nothing, Int]
    lazy val live: ZLayer[Any, Nothing, Google] =
      ZLayer.succeed(make)
    lazy val make: Google.Service = new:
      override def count(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cat" then 1 else 2)

object bizLogic:
  type BizLogic = Has[BizLogic.Service]
  object BizLogic:
    trait Service:
      def isGoogleResultEven(topic: String): ZIO[Any, Nothing, Boolean]
    lazy val live: ZLayer[google.Google, Nothing, BizLogic] =
      ZLayer.fromService(make)
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
    lazy val live
        : ZLayer[bizLogic.BizLogic & console.Console, Nothing, Controller] =
      ZLayer.fromServices(make)

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
  lazy val run: ZIO[controller.Controller, Nothing, Unit] =
    ZIO.accessM(_.get[Controller.Service].run)

object MainZioInjection extends scala.App:
  Runtime.default.unsafeRunSync(
    program.flatMap(_.get[controller.Controller.Service].run)
    // controller.run.provide(program)
  )

  lazy val program =
    (for
      (g, con) <- google.Google.live.zio.zip(console.Console.live.zio)
      bl <- bizLogic.BizLogic.live.zio.provide(g)
      c <- controller.Controller.live.zio.provide(bl ++ con)
    yield c)
