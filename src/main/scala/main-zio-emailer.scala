package zioEmailer

import zio.*
import scala.collection.immutable.LazyList.cons
import org.scalafmt.config.DanglingParentheses.Exclude.`trait`

case class User(name: String, email: String)

object UserEmailer:
  trait Service:
    def notify(user: User, message: String): Task[Unit]
  lazy val live = ZLayer.fromFunction(make)
  def make(console: Console): Service = new Service:
    override def notify(user: User, message: String): Task[Unit] =
      console.printLine(
        scala.Console.RED + s"[user emailer] notify user: ${user.name} with $message" + scala.Console.RESET
      )

object UserDb:
  trait Service:
    def insert(user: User): Task[Unit]
  lazy val live = ZLayer.fromFunction(make)
  def make(console: Console): Service = new Service:
    override def insert(user: User): Task[Unit] =
      console.printLine(
        scala.Console.RED + s"[user db] inserted user ${user.name}" + scala.Console.RESET
      )
object SubscriptionService:
  trait Service:
    def subscribe(user: User): Task[User]

  lazy val live = ZLayer.fromFunction(make)

  def make(ue: UserEmailer.Service, udb: UserDb.Service): Service = new:
    override def subscribe(user: User): Task[User] =
      for
        _ <- udb.insert(user)
        _ <- ue.notify(user, s"\n[subscription service] welcome ${user.name}")
      yield user

object MainEmailer extends ZIOAppDefault:
  val wenUser = User("Wenshuai Hou", "wenhoujx@gmail.com")
  lazy val run = program
    .provide(
      SubscriptionService.live,
      UserEmailer.live,
      UserDb.live,
      ZLayer.succeed(Console.ConsoleLive)
    )
    .exitCode
  lazy val program =
    for
      console <- ZIO.service[Console]
      sub <- ZIO.service[SubscriptionService.Service]
      _ <- console.printLine("-" * 50)
      _ <- sub.subscribe(wenUser)
      _ <- console.printLine("-" * 50)
    yield ()
