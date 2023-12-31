package zio2layer
// this ap showcase the awesomeness of zio 2 layer.
// https://www.youtube.com/watch?v=3ScqDZp9X3c&list=PPSV
import zio.*
import java.io.IOException
import myzio.console
import scala.collection.immutable.LazyList.cons
import myzio.ZIO.fromFunction

trait Boundary:
  def hasEvenCount(topic: String): UIO[Boolean]
trait Google:
  def count(topic: String): UIO[Int]
trait Controller:
  def run: IO[IOException, Unit]

object BoundaryImpl:
  lazy val live = ZLayer.fromFunction(make)
  def make(g: Google): Boundary =
    new:
      override def hasEvenCount(topic: String): UIO[Boolean] =
        g.count(topic).map(x => if x % 2 == 0 then true else false)
object GoogleImpl:
  lazy val live = ZLayer.succeed(make)
  lazy val make: Google = new:
    override def count(topic: String): UIO[Int] =
      ZIO.succeed(if topic == "cat" then 1 else 2)

def fancyPrintLine(console: Console, line: => String): IO[IOException, Unit] =
  console.printLine(scala.Console.GREEN + line + scala.Console.RESET)

object ControllerImpl:
  lazy val live = ZLayer.fromFunction(make)
  def make(boundary: Boundary, console: Console): Controller = new:
    override def run: ZIO[Any, IOException, Unit] =
      for
        _ <- fancyPrintLine(console, "-" * 50)
        _ <- boundary
          .hasEvenCount("cat")
          .flatMap(x => fancyPrintLine(console, x.toString))
        _ <- boundary
          .hasEvenCount("dog")
          .flatMap(x => fancyPrintLine(console, x.toString))
        _ <- fancyPrintLine(console, "-" * 50)
      yield ()

object program:
  lazy val makeWithConstructors = ZIO.succeed(
    ControllerImpl.make(BoundaryImpl.make(GoogleImpl.make), Console.ConsoleLive)
  )
  lazy val makeWithZioLayers = ZIO
    .service[Controller]
    .provideSome[Console](
      ControllerImpl.live,
      BoundaryImpl.live,
      GoogleImpl.live
    )

object MainZio2Layer extends ZIOAppDefault:
  lazy val run = program.makeWithZioLayers
    .provide(ZLayer.succeed(Console.ConsoleLive))
    .flatMap(_.run)
