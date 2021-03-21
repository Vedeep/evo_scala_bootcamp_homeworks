package com.evobootcamp.homeworks.cats

import cats.Functor

import scala.annotation.tailrec

object Excercises {
  trait Applicative[F[_]] extends Functor[F] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
      = apply(unit(f))(fa)

    def unit[A](a: => A): F[A]

    // implement methods using each other
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
      = apply(map(fa)(a => (b: B) => f(a, b)))(fb)

    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

    def sequence[A](fas: List[F[A]]): F[List[A]] = {
      @tailrec
      def makeList(list: F[List[A]], fas: List[F[A]]): F[List[A]] = {
        fas match {
          case Nil => list
          case x :: xs => makeList(map2(x, list)(_ :: _), xs)
        }
      }

      makeList(unit(List()), fas)
    }

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    // implement methods using each other
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

    def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(a => a)

    def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

    def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma) { a =>
      map(mb) { b =>
        f(a, b)
      }
    }
  }
}
