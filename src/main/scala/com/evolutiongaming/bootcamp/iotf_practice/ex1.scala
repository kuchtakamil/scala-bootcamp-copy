package com.evolutiongaming.bootcamp.iotf_practice

import cats.{Applicative, Show}
import cats.effect.IO
import cats.effect.kernel.{CancelScope, Poll, Sync}
import cats.effect.std.Console

import java.nio.charset.Charset
import scala.concurrent.duration.FiniteDuration

object ex1 {

  sealed trait State
  case object Open   extends State
  case object Closed extends State

  trait Metrics[F[_]] {
    def increase: F[Unit]
    def decrease: F[Unit]
    def gaugeTo(state: State): F[Unit]
  }
  object Metrics {

    import cats.syntax.applicative._
    /*
     * Task: implement, should do nothing
     */
    def empty[F[_]: Applicative]: Metrics[F] = new Metrics[F] {
      def increase: F[Unit]              = ().pure // Applicative[F].pure(())
      def decrease: F[Unit]              = ().pure // Applicative[F].pure(())
      def gaugeTo(state: State): F[Unit] = ().pure // Applicative[F].pure(())
    }

    val console: Console[IO] = new Console[IO] {
      override def readLineWithCharset(charset: Charset): IO[String] = ???
      override def print[A](a: A)(implicit S: Show[A]): IO[Unit] = ???
      override def println[A](a: A)(implicit S: Show[A]): IO[Unit] = ???
      override def error[A](a: A)(implicit S: Show[A]): IO[Unit] = ???
      override def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = ???
    }

    /*
     * Task: implement, should print to console "increase", "decrease", and s"gauge: $state"
     */
    def console[F[_]: Console]: Metrics[F] = new Metrics[F] {
      //      def increase: F[Unit]              = Sync[F].delay { println("increase") }
      def increase: F[Unit]              = Console[F].println { "increase" }
      def decrease: F[Unit]              = Console[F].println { "decrease" }
      //      def gaugeTo(state: State): F[Unit] = Sync[F].delay {
      def gaugeTo(state: State): F[Unit] = Console[F].println {
        throw new RuntimeException("")
      }
    }
  }

}
