package com.evolutiongaming.bootcamp.iotf_practice

import cats.{Id, Monad, Applicative}
import cats.effect.{IO, Sync}

import java.io.FileWriter

object Intro {
  /*
   * Let's look at two simple functions:
   * Question: what can we say about their operation in the first and second cases?
   */
  def parse1(in: Any): Any            = ???

  /**
    * Parse the int
    */
  def parse2(in: String): Option[Int] = ???










  /*
   * Let's look at a more complicated example:
   * Question: what can we say about their operation in the first and second cases?
   */
  object `1` {
    trait Log {
      def info(message: String): IO[Unit]
    }

    case class Response[A](status: Int, data: A)
    trait Send {
      def toEveryone(id: Long): IO[Response[Boolean]]
    }

    def logAndSend(id: Long, log: Log, send: Send): IO[Boolean] =
      for {
        _ <- log.info(s"start $id")
        result <- send.toEveryone(id)
        _ <- IO.delay {
          throw new RuntimeException("Ops")
        }
        _ <- IO.delay {
          val writer = new FileWriter("opa.txt")
          writer.write(result.status.toString)
          writer.close()
        }
        _ <- log.info(s"end with ${result.status}")
      } yield result.data
  }







  object `2` {
    def function[F[_]: Log](): F[Unit] = Log[F].info("asdasda")

    trait Log[F[_]] {
      def info(message: String): F[Unit]
    }
    object Log {
      def empty[F[_]: Applicative]: Log[F] = new Log[F] {
        def info(message: String): F[Unit] = Applicative[F].pure(())
      }

//      because implicitly used
//      def apply[F[_]](implicit a: Log[F]): Log[F] = a
    }

    case class Response[A](status: Int, data: A)
    trait Send[F[_]] {
      def toEveryone(id: Long): F[Response[Boolean]]
    }
    object Send {
      val stub: Send[IO] = new Send[IO] {
        def toEveryone(id: Long): IO[Response[Boolean]] = IO.pure(Response(200, true))
      }

      def apply[F[_]](implicit a: Send[F]): Send[F] = a
    }

    import cats.syntax.all._
    def logAndSend[F[_]: Log: Send: Monad](id: Long): F[Boolean] =
      for {
        _ <- implicitly[Log[F]].info("Sending")
        response <- Send[F].toEveryone(id)
        _ <- implicitly[Log[F]].info(response.toString)
      } yield response.data

//    def logAndSend[F[_]: Monad](id: Long)(implicit log: Log[F], send: Send[F]): F[Boolean] = ???
  }
}


