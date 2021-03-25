package com.evobootcamp.homeworks.cats

object FiveFundamentalMonads {
  import scala.language.higherKinds

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }


  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] = amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] = ma match {
        case None => None
        case Just(a) => amb(a)
      }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    // inspired by https://stackoverflow.com/a/6248296
    implicit def stateMonad[S] = new Monad[({type x[a]=State[S, a]})#x] {
      def unit[A](a: A): State[S, A] = State(s => (s, a))
      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] = {
        State(s => {
          val (s2, a) = ma.run(s)
          amb(a).run(s2)
        })
      }
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    implicit def readerMonad[R] = new Monad[({type x[a]=Reader[R, a]})#x] {
      def unit[A](a: A): Reader[R, A] = Reader(r => a)
      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] = {
        Reader(r1 => {
          amb(ma.run(r1)).run(r1)
        })
      }
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[({type x[a]=Writer[W, a]})#x] {
      def unit[A](a: A): Writer[W, A] = Writer((m.mempty, a))
      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] = {

        ma match {
          case Writer((w, a)) => {
            amb(a) match {
              case Writer((w2, a2)) => {
                Writer(m.mappend(w)(w2), a2)
              }
            }
          }
        }

      }
    }
  }
}

