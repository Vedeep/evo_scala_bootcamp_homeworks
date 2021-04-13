package com.evobootcamp.homeworks.effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, Fiber, IO, IOApp, Timer}
import cats.implicits._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
    state: Ref[F, Map[K, (Long, V)]],
    expiresIn: FiniteDuration
  )(implicit TF: Timer[F]) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]]
      = state.get.map(_.get(key).map(_._2))

    def put(key: K, value: V): F[Unit] = {
      for {
        ts <- Clock[F].realTime(TimeUnit.MILLISECONDS)
        _ <- state.getAndUpdate(map => map + (key -> (ts + expiresIn.toMillis, value)))
      } yield ()
    }
  }

  object Cache {
    def of[F[_] : Clock : Monad, K, V](
      expiresIn: FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      for {
        cacheMap <- Ref.of[F, Map[K, (Long, V)]](Map.empty)
        cache <- Concurrent[F].pure(new RefCache[F, K, V](cacheMap, expiresIn))
        _ <- startCheckInterval[F, K, V](cacheMap, checkOnExpirationsEvery)
      } yield cache
    }

    def startCheckInterval[F[_] : Clock : Monad, K, V]
    (cache: Ref[F, Map[K, (Long, V)]], exp: FiniteDuration)
    (implicit T: Timer[F], C: Concurrent[F]): F[Fiber[F, Unit]] = {
      val timer: F[Unit] = Timer[F].sleep(exp).flatMap(_ => {
        for {
          ts <- Clock[F].realTime(TimeUnit.MILLISECONDS)
          _ <- cache.getAndUpdate(_.filterNot(_._2._1 < ts))
        } yield ()
      }).foreverM

      Concurrent[F].start(timer)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- IO.pure(println(cache))
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success
  }
}

