package myzio
import myzio.console
import java.io.IOException
import myzio.*

object FancyConsole:
  
  lazy val live: ZLayer[Any, Nothing, console.Console] =
    ZLayer.succeed(make)

  lazy val make: console.Console.Service =
    new:
      override def getLine: ZIO[Any, IOException, String] =
        ZIO.succeed(scala.io.StdIn.readLine())
      override def printLine(line: => String): ZIO[Any, IOException, Unit] =
        ZIO.succeed(println(scala.Console.GREEN + line + scala.Console.RESET))
