package myzio

import java.io.IOException
object google:
  type Google = Has[Google.Service]
  object Google:
    trait Service:
      def count(topic: String): ZIO[Any, Nothing, Int]
    lazy val any: ZLayer[Google, Nothing, Google] = ZLayer.requires
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
    lazy val any: ZLayer[BizLogic, Nothing, BizLogic] = ZLayer.requires
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
      def run: ZIO[Any, IOException, Unit]
    lazy val any: ZLayer[Controller, Nothing, Controller] = ZLayer.requires
    lazy val live
        : ZLayer[bizLogic.BizLogic & console.Console, Nothing, Controller] =
      ZLayer.fromServices(make)

    def make(
        bl: bizLogic.BizLogic.Service,
        con: console.Console.Service
    ): Service =
      new:
        override def run: ZIO[Any, IOException, Unit] =
          for
            _ <- con.printLine("-" * 50)
            cat <- bl.isGoogleResultEven("cat")
            _ <- con.printLine(cat.toString)
            dog <- bl.isGoogleResultEven("dog")
            _ <- con.printLine(dog.toString)
            _ <- con.printLine("-" * 50)
          yield ()
  lazy val run: ZIO[controller.Controller, IOException, Unit] =
    ZIO.accessM(_.get[Controller.Service].run)

object MainZioInjection extends scala.App:
  Runtime.default.unsafeRunSync(
    // program.zio.flatMap(_.get[controller.Controller.Service].run)
    // controller.run.provideLayer(FancyConsole.live >>> layer)
    controller.run.provideSomeLayer(layer).provideLayer(FancyConsole.live)
  )

  lazy val layer: ZLayer[console.Console, Nothing, controller.Controller] =
    val bizLayer: ZLayer[Any, Nothing, bizLogic.BizLogic] =
      google.Google.live >>> bizLogic.BizLogic.live

    (bizLayer ++ (console.Console.any)) >>> controller.Controller.live
