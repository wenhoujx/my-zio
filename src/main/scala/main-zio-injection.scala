package myzio

import scala.collection.immutable.LazyList.cons
import zio.http.Decompression.No

object google:
  type Google = Has[Google.Service]
  object Google:
    trait Service:
      def count(topic: String): ZIO[Any, Nothing, Int]
    lazy val live: ZIO[Any, Nothing, Google] =
      ZIO.fromFunction(_ => Has(make))
    lazy val make: Google.Service = new:
      override def count(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cat" then 1 else 2)

object bizLogic:
  type BizLogic = Has[BizLogic.Service]
  object BizLogic:
    trait Service:
      def isGoogleResultEven(topic: String): ZIO[Any, Nothing, Boolean]
    lazy val live: ZIO[google.Google, Nothing, BizLogic] =
      ZIO.fromFunction(env => Has(make(env.get[google.Google.Service])))
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
        : ZIO[bizLogic.BizLogic & console.Console, Nothing, Controller] =
      ZIO.fromFunction { env =>
        val bl = env.get[bizLogic.BizLogic.Service]
        val con = env.get[console.Console.Service]
        Has(make(bl, con))
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
  Runtime.default.unsafeRunSync(program)

  lazy val program =
    (for
      g <- google.Google.live
      bl <- bizLogic.BizLogic.live.provide(g)
      con <- console.Console.live
      c <- controller.Controller.live.provide(bl ++ con)
    yield c).flatMap(_.get[controller.Controller.Service].run)
