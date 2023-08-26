package com.evolutiongaming.bootcamp.iotf_practice

import cats.data.ReaderT
import cats.effect.IO
import cats.effect.std.Console
import cats.parse.Parser.Expectation.WithContext
import cats.{Applicative, Monad, MonadError, MonadThrow}
import cats.syntax.all._

object ex4 {
  case class Message(value: String)

  trait Send[F[_]] {

    /** Send the message to all specified players by their ids
      * @return
      *   may return connection errors
      */
    def send(ids: Set[Long], msg: Message): F[Unit]
  }

  trait AllIdsCache[F[_]] {

    /** @return
      *   always returns set of ids of players
      */
    def get: F[Set[Long]]
  }

  /*
   * We also have additional traits to be able handle and raise specific errors
   */
  trait Raise[F[_], E] {
    def raise(error: E): F[Unit]
  }
  trait Handle[F[_], E] {
    def handle[A](fa: F[A]): F[Either[E, A]]
  }

  // import tofu
  //

  type Context = String
  type Data = Int
  val computation: ReaderT[IO, Context, Data] = ReaderT {
    context => IO.pure(1)
  }

  def f[F[_]: Monad](): F[Unit] = ???

  /*
    def g[F[_]: Monad: WithContext[*[_], Context]](): F[Unit] =
      for {
        context <- WithContext[F, Context].getContext
      } yield ()
  */

  /*
   * We have special Error class which will be handled at the end of our service
   */
  case class ApiError(status: Int, message: String) extends Throwable
  object ApiError {
    def from(error: Throwable): ApiError = ApiError(500, error.getMessage)
  }

  /** Sends messages to players. Handles all connection errors, logs them, and can return only ApiErrors
    */
  trait SendTo[F[_]] {
    def toEveryone(msg: Message): F[Unit]
    def toPlayer(id: Long, msg: Message): F[Unit]
  }
  object SendTo {
    /*
     * Task: improve SendTo implementation to complete conditions from the comment
     */
    def make[F[_]: MonadThrow: Console](
                                         cache: AllIdsCache[F],
                                         send: Send[F],
                                       )(implicit handle: Handle[F, Throwable], raise: Raise[F, ApiError]): SendTo[F] =
      new SendTo[F] {
        def toEveryone(msg: Message): F[Unit] =
          cache.get.flatMap { ids =>
            sendSafe(ids, msg)
          }// *> MonadThrow[F].raiseWhen(true)(new RuntimeException())

        def toPlayer(id: Long, msg: Message): F[Unit] =
          sendSafe(Set(id), msg)

        def sendSafe(ids: Set[Long], msg: Message): F[Unit] =
          handle.handle(send.send(ids, msg)).flatMap {
            case Left(error) => Console[F].errorln(s"error: $error") >> raise.raise(ApiError.from(error))
            case Right(_)    => Applicative[F].unit
          }
      }
  }


  implicit val ioHandle: Handle[IO, Throwable] = ???
  implicit val eitherRaise: Raise[Either[Throwable, *], ApiError] = ???

  /*
   * Homework: write a test for `SendTo` service using `cats.IO` and `Either[Throwable, *]`
   * You will have to implement `Handle` and `Raise` for them and test that `SendTo` uses them correctly
   */
}
