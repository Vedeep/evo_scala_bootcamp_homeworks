package com.evobootcamp.homeworks.effects

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */

object EffectsHomework1 extends App {
  final class IO[A](body: => A) {
    private[effects] val run: () => A = () => body

    def map[B](f: A => B): IO[B] = IO(f(body))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(map(f).run().run())
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = map(_ => ())
    def attempt: IO[Either[Throwable, A]] = redeem(Left.apply, Right.apply)
    def option: IO[Option[A]] = redeem(_ => None, Option.apply)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA]
      = redeemWith(f, a => IO.pure(a))
    def redeem[B](recover: Throwable => B, map: A => B): IO[B]
      = redeemWith(e => IO(recover(e)), v => IO(map(v)))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B]
      = IO.unit.flatMap(_ => Try(run()).fold(recover, bind))
    def unsafeRunSync(): A = body
    def unsafeToFuture(): Future[A] = Future.fromTry(Try(unsafeRunSync))
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(body)
    def suspend[A](thunk: => IO[A]): IO[A] = unit.flatMap(_ => thunk)
    def delay[A](body: => A): IO[A] = apply(body)
    def pure[A](a: A): IO[A] = apply(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(e)  => raiseError(e)
      case Right(v) => pure(v)
    }
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case None        => raiseError(orElse)
      case Some(value) => pure(value)
    }
    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(e) => raiseError(e)
      case Success(v) => pure(v)
    }
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = apply(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit]
      = unlessA(cond)(raiseError(e))
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit]
      = whenA(cond)(raiseError(e))
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit]
      = if (cond) IO.unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit]
      = if (cond) action else IO.unit
    val unit: IO[Unit] = pure(())
  }
}
