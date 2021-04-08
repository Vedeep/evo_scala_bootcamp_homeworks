package com.evobootcamp.homeworks.effects

import cats.{Applicative, ApplicativeError, Apply, Monad, MonadError}
import cats.effect.{Blocker, ContextShift, ExitCode, Fiber, IO, IOApp, Resource, Sync}
import cats.implicits._

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.io.{Source, StdIn}
import scala.util.control.{NoStackTrace, NonFatal}

/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.
  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
*/

object EffectsHomework2App extends IOApp {
  import EffectsHomework2._

  private val pool = new PoolImpl[IO] {
    override def start[A](fa: IO[A]): IO[Fiber[IO, A]] = fa.start
  }
  private val console = new ConsoleImpl[IO](pool)
  private val fileReader = new FileReaderImpl[IO]
  private val validator = new ValidatorImpl
  private val hash = new HashImpl[IO](pool)
  private val storage = new HashStorage[IO]

  private val app: HashApp[IO] = new HashApp[IO](
    fileReader,
    console,
    validator,
    hash,
    storage
  )

  override def run(args: List[String]): IO[ExitCode] = for {
    minHash <- app.run()
    _ <- console.printLine(minHash.toString)
    _ <- pool.shutdown()
  } yield ExitCode.Success
}

object EffectsHomework2 {
  sealed case class HashItem(text: String, hash: Int)

  sealed trait ValidationError extends Throwable with NoStackTrace
  object ValidationError extends Throwable with NoStackTrace {
    case object EmptyString extends ValidationError
    case object NotNumber extends ValidationError
    case object EmptyFile extends ValidationError
  }

  sealed trait Console[F[_]] {
    def readLine: F[Fiber[F, String]]
    def printLine(text: String): F[Unit]
  }

  sealed trait Validator {
    def validateFileName(fileName: String): Either[ValidationError, String]
    def validateSeed(seed: String): Either[ValidationError, Int]
    def validateContent(data: String): Either[ValidationError, String]
  }

  sealed trait FileReader[F[_]] {
    def readFile(fileName: String): F[String]
  }

  sealed trait Hash[F[_], A] {
    def getMinHash(text: String, seed: Int): F[A]
  }

  sealed trait ThreadPool[F[_]] {
    def delay[A](thunk: => A)(implicit sync: Sync[F], cs: ContextShift[F]): F[Fiber[F, A]]
    def shutdown(): F[Unit]
  }

  sealed trait Storage[F[_], T] {
    def set(key: String, item: T): F[Unit]
    def get(key: String): F[Option[T]]
  }

  sealed class HashStorage[F[_]](implicit ap: Applicative[F]) extends Storage[F, HashItem] {
    val store: AtomicReference[Map[String, HashItem]] = new AtomicReference(Map())

    override def set(key: String, item: HashItem): F[Unit] = {
      Applicative[F].pure(store.getAndUpdate(_ + (key -> item)))
    }

    override def get(key: String): F[Option[HashItem]] = {
      Applicative[F].pure(store.get().get(key))
    }
  }

  abstract class PoolImpl[F[_]]
  (private val poolSize: Int = 4)
  (implicit ap: Applicative[F])
  extends ThreadPool[F] {
    private[effects] val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
    private[effects] val executionContext: ExecutionContextExecutor
      = ExecutionContext.fromExecutor(pool)
    private[effects] val blocker = Blocker.liftExecutionContext(executionContext)

    def start[A](fa: F[A]): F[Fiber[F, A]]

    override def delay[A](thunk: => A)(implicit sync: Sync[F], cs: ContextShift[F]): F[Fiber[F, A]]
      = start(blocker.delay[F, A](thunk))

    override def shutdown(): F[Unit] = Applicative[F].pure(pool.shutdown)
  }

  class ConsoleImpl[F[_] : Applicative](val pool: ThreadPool[F])(implicit sync: Sync[F], cs: ContextShift[F])
  extends Console[F] {
    override def readLine: F[Fiber[F, String]]
      = pool.delay(StdIn.readLine)

    override def printLine(text: String): F[Unit]
      = Applicative[F].pure(println(text))
  }

  class FileReaderImpl[F[_]](implicit sync: Sync[F], ap: Applicative[F]) extends FileReader[F] {
    override def readFile(fileName: String): F[String] = {
      (for {
        file <- Resource.fromAutoCloseable(Applicative[F].pure(Source.fromFile(fileName)))
      } yield file).use({ file =>
        for {
          data <- Applicative[F].pure(file.getLines().toList.mkString(" "))
        } yield data
      })
    }
  }

  class ValidatorImpl extends Validator {
    override def validateFileName(fileName: String): Either[ValidationError, String]
      = Either.cond(fileName.nonEmpty, fileName, ValidationError.EmptyString)

    override def validateSeed(seed: String): Either[ValidationError, Int]
      = seed.toIntOption match {
      case None => Left(ValidationError.NotNumber)
      case Some(v) => Right(v)
    }

    override def validateContent(data: String): Either[ValidationError, String]
      = Either.cond(data.nonEmpty, data, ValidationError.EmptyFile)
  }

  class HashImpl[F[_]](pool: ThreadPool[F])(implicit sync: Sync[F], cs: ContextShift[F], ap: Applicative[F])
  extends Hash[F, HashItem] {
    override def getMinHash(text: String, seed: Int): F[HashItem] = for {
      words <- Applicative[F].pure(text.split("[\\s\\t\\n]+").toList)
      hashes <- words.traverse(word => getWordHash(word, seed).map(HashItem(word, _)))
    } yield hashes.sortWith(_.hash < _.hash).head

    def getWordHash(word: String, seed: Int): F[Int] = {
      for {
        f1 <- pool.delay(javaHash(word))
        f2 <- pool.delay(knuthHash(word, seed))

        h1 <- f1.join
        h2 <- f2.join
      } yield (h1, h2).minimum
    }

    def javaHash(word: String): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = 31 * hash + ch.toInt
      hash = hash ^ (hash >> 20) ^ (hash >> 12)
      hash ^ (hash >> 7) ^ (hash >> 4)
    }
    def knuthHash(word: String, constant: Int): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
      hash % constant
    }
  }

  class HashApp[F[_]](
    val fileReader: FileReader[F],
    val console: Console[F],
    val validator: Validator,
    val hash: Hash[F, HashItem],
    val storage: Storage[F, HashItem]
  )(implicit sync: Sync[F], cs: ContextShift[F], met: MonadError[F, Throwable]) {
    private val storageKey: String = "min-hash"
    private def storeMinHash(hash: HashItem): F[Unit]
      = storage.set(storageKey, hash)

    def run(): F[HashItem] = {
      (for {
        _ <- console.printLine("Enter file name:")
        fiber1 <- console.readLine
        dirtyName <- fiber1.join
        fileName <- MonadError[F, Throwable].fromEither(validator.validateFileName(dirtyName))

        _ <- console.printLine("Enter seed:")
        fiber2 <-  console.readLine
        dirtySeed <- fiber2.join
        seed <- MonadError[F, Throwable].fromEither(validator.validateSeed(dirtySeed))

        dirtyData <- fileReader.readFile(fileName)
        data <- MonadError[F, Throwable].fromEither(validator.validateContent(dirtyData))
        minHash <- hash.getMinHash(data, seed)

        _ <- storeMinHash(minHash)
      } yield minHash)
        .handleErrorWith(e => {
          e match {
            case ValidationError.NotNumber
              => console.printLine(s"Error: Seed should be a number in range ${Int.MinValue} - ${Int.MaxValue}")
            case ValidationError.EmptyString => console.printLine("Error: Filename can't be empty")
            case ValidationError.EmptyFile   => console.printLine("Error: File is empty")
            case NonFatal(e)                 => console.printLine(s"Error: ${e.getMessage}")
          }

          run
        })
    }
  }
}
