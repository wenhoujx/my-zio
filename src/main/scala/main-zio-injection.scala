package myzio

import scala.collection.immutable.LazyList.cons

trait Google:
  def count(topic: String): ZIO[Any, Nothing, Int]
object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] = ZIO.fromFunction(_ => make)
  lazy val make: Google = new:
    override def count(topic: String): ZIO[Any, Nothing, Int] =
      ZIO.succeed(if topic == "cat" then 1 else 2)

object bizLogic:
  trait BizLogic:
    def isGoogleResultEven(topic: String): ZIO[Any, Nothing, Boolean]

  object BizLogic:
    lazy val live: ZIO[Google, Nothing, BizLogic] = ZIO.fromFunction(make)
    def make(g: Google): BizLogic = new:
      override def isGoogleResultEven(
          topic: String
      ): ZIO[Any, Nothing, Boolean] =
        g.count(topic).map(_ % 2 == 0)

  def isGoogleResultEven(topic: String): ZIO[BizLogic, Nothing, Boolean] =
    ZIO.accessM(_.isGoogleResultEven(topic))

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, bizLogic.BizLogic] =
    for {
      g <- GoogleImpl.live
      bl <- bizLogic.BizLogic.live.provide(g)
    } yield bl
  lazy val make: bizLogic.BizLogic = bizLogic.BizLogic.make(GoogleImpl.make)

object MainZioInjection extends scala.App:
  Runtime.default.unsafeRunSync(
    for
      g <- GoogleImpl.live
      bl <- bizLogic.BizLogic.live.provide(g)
      p <- program.provideSome[Has[ZENV]](Has(bl) union _)
    yield p
  )

  lazy val program =
    for
      env <- ZIO.identity[Has[console.Console] & Has[bizLogic.BizLogic]]
      _ <- env.get[console.Console].printLine("-" * 50)
      cat <- env.get[bizLogic.BizLogic].isGoogleResultEven("cat")
      _ <- env.get[console.Console].printLine(cat.toString)
      dog <- env.get[bizLogic.BizLogic].isGoogleResultEven("dog")
      _ <- env.get[console.Console].printLine(dog.toString)
      _ <- env.get[console.Console].printLine("-" * 50)
    yield ()
