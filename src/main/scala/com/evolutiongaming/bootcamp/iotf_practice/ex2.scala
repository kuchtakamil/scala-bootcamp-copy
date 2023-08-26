package com.evolutiongaming.bootcamp.iotf_practice

import cats.{FlatMap, Monad}
import cats.effect.{IO, Sync}
import cats.implicits._
import com.evolutiongaming.bootcamp.typeclass.v3_typeclass.TypeClassesExamples.Applicative

object ex2 {

  sealed trait State
  case object Open   extends State
  case object Closed extends State

  trait Metrics[F[_]] {
    def increase: F[Unit]
    def decrease: F[Unit]
    def gaugeTo(state: State): F[Unit]
  }

  object Metrics {
    /*
     * Task: implement, should use provider
     */
    new Monad[IO] {
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B]        = ???
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
      override def pure[A](x: A): IO[A]                                  = ???
    }

    new Applicative[IO] {
      override def pure[A](x: A): IO[A] = ???

      override def product[A, B](fa: IO[A], fb: IO[B]): IO[(A, B)] = ???

      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = ???
    }

    new FlatMap[IO] {
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = ???
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = ???
      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = ???
    }

    def make[F[_]: FlatMap](provider: PrometheusProvider[F]): Metrics[F] = new Metrics[F] { // or Monad
      def increase: F[Unit]              = provider.getCounter.flatMap { value =>
        provider.setCounter(value + 1)
      }
      def decrease: F[Unit]              = provider.getCounter.flatMap { value =>
        provider.setCounter(value - 1)
      }
      def gaugeTo(state: State): F[Unit] = provider.setGauge(state.toString)
    }
  }

  /* Let's assume that it is a library code */
  trait PrometheusProvider[F[_]] {
    def getCounter: F[Int]
    def setCounter(i: Int): F[Unit]
    def setGauge(state: String): F[Unit]
  }

}
