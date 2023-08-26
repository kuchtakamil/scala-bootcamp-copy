package com.evolutiongaming.bootcamp.iotf_practice

import cats.{Id, effect}
import cats.syntax.all._
import cats.effect.{Async, IO}
import cats.effect.kernel.{Deferred, Sync}
import zio.{Task, ZIO}

import scala.concurrent.Future

object ex3 {
  case class User(id: Long, login: String)

  /*
   * Old service from the company-common library
   */
  trait UnsafeUserRepository {
    def findUser(id: Long): Future[Option[User]]
    def addUser(user: User): Future[Unit]
  }
  val unsafe: UnsafeUserRepository = ???

  trait UserRepository[F[_]] {
    def findUser(id: Long): F[Option[User]]
    def addUser(user: User): F[Unit]
  }
  object UserRepository {
    /*
     * Task: implement so that the service can be used from both `cats.IO` and `zio.Task`
     */
    def make[F[_]: FromFuture](unsafe: UnsafeUserRepository): UserRepository[F] =
      new UserRepository[F] {
        def findUser(id: Long): F[Option[User]] = FromFuture[F].apply {
          unsafe.findUser(id)
        }
        def addUser(user: User): F[Unit]        = FromFuture[F].apply {
          unsafe.addUser(user)
        }
      }
  }

  val userRepository:  UserRepository[IO] = UserRepository.make[IO](unsafe)
  val userRepository2:  UserRepository[Task] = UserRepository.make[Task](unsafe)
  val userRepository3:  UserRepository[Id] = UserRepository.make[Id](unsafe)

  // Hint:
  def futureToIO[A](f: => Future[A]): IO[A]    = IO.fromFuture(IO.delay(f))
  def futureToZIO[A](f: => Future[A]): Task[A] = ZIO.fromFuture(_ => f)

  trait FromFuture[F[_]] {
    def apply[A](f: => Future[A]): F[A]
  }
  object FromFuture {
    def apply[F[_]](implicit a: FromFuture[F]): FromFuture[F] = a
  }

  /* Task: FromFuture[IO], FromFuture[Task] */
  implicit val io: FromFuture[IO] = new FromFuture[IO] {
    def apply[A](f: => Future[A]): IO[A] = IO.fromFuture(IO.delay(f))
  }
  implicit val zio: FromFuture[Task] = new FromFuture[Task] {
    def apply[A](f: => Future[A]): Task[A] = ZIO.fromFuture(_ => f)
  }


  /*
   * Bonus task: make it possible to create the service from `cats.Id` for testing purposes
   */
}
