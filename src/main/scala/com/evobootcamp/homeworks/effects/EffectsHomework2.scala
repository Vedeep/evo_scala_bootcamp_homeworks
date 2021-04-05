package com.evobootcamp.homeworks.effects

import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Resource, Sync}
import cats.implicits._

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

  private val pool = new PoolImpl
  private val console = new ConsoleImpl(pool)
  private val fileReader = new FileReaderImpl
  private val validator = new ValidatorImpl
  private val hash = new HashImpl(pool)
  private val storage = new HashStorage

  private val app = new HashApp(
    fileReader,
    console,
    validator,
    hash,
    storage
  )

  override def run(args: List[String]): IO[ExitCode]
    = app.run() *> pool.shutdown() *> IO.pure(ExitCode.Success)
}

object EffectsHomework2 {
  type HashItem = (String, Int)

  sealed trait ValidationError extends Throwable with NoStackTrace
  object ValidationError extends Throwable with NoStackTrace {
    case object EmptyString extends ValidationError
    case object NotNumber extends ValidationError
    case object EmptyFile extends ValidationError
  }

  sealed trait Console {
    def readLine: IO[String]
    def printLine(text: String): IO[Unit]
  }

  sealed trait Validator {
    def validateFileName(fileName: String): Either[ValidationError, String]
    def validateSeed(seed: String): Either[ValidationError, Int]
    def validateContent(data: String): Either[ValidationError, String]
  }

  sealed trait FileReader {
    def readFile(fileName: String): IO[String]
  }

  sealed trait Hash {
    def getMinHash(text: String, seed: Int): IO[HashItem]
  }

  sealed trait ThreadPool {
    def delay[A](thunk: => A)(implicit sync: Sync[IO], cs: ContextShift[IO]): IO[A]
    def shutdown(): IO[Unit]
  }

  sealed trait Storage[T] {
    def add(key: String, item: T): IO[Unit]
    def get(key: String): IO[Option[T]]
  }

  sealed class HashStorage extends Storage[HashItem] {
    @volatile var store: Map[String, HashItem] = Map()

    override def add(key: String, item: HashItem): IO[Unit] = {
      IO.pure(synchronized {
        store = store + (key -> item)
      })
    }

    override def get(key: String): IO[Option[HashItem]] = {
      IO.pure(synchronized {
        store.get(key)
      })
    }
  }

  class PoolImpl(private val poolSize: Int = 4) extends ThreadPool {
    private val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
    private val executionContext: ExecutionContextExecutor
      = ExecutionContext.fromExecutor(pool)
    private val blocker = Blocker.liftExecutionContext(executionContext)

    override def delay[A](thunk: => A)(implicit sync: Sync[IO], cs: ContextShift[IO]): IO[A]
      = blocker.delay(thunk)

    override def shutdown(): IO[Unit] = IO(pool.shutdown)
  }

  class ConsoleImpl(val pool: ThreadPool)(implicit sync: Sync[IO], cs: ContextShift[IO]) extends Console {
    override def readLine: IO[String]
      = pool.delay(StdIn.readLine)

    override def printLine(text: String): IO[Unit]
      = IO.pure(println(text))
  }

  class FileReaderImpl extends FileReader {
    override def readFile(fileName: String): IO[String] = {
      (for {
        file <- Resource.fromAutoCloseable(IO(Source.fromFile(fileName)))
      } yield file).use({ file =>
        for {
          data <- IO.pure(file.getLines().toList.mkString(" "))
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

  class HashImpl(pool: ThreadPool)(implicit sync: Sync[IO], cs: ContextShift[IO]) extends Hash {
    override def getMinHash(text: String, seed: Int): IO[HashItem] = for {
      words <- IO.pure(text.split("[\\s\\t\\n]+").toList)
      hashes <- words.traverse(word => getWordHash(word, seed).map((word, _)))
    } yield hashes.sortWith(_._2 < _._2).head

    def getWordHash(word: String, seed: Int): IO[Int] = {
      for {
        f1 <- pool.delay(javaHash(word)).start
        f2 <- pool.delay(knuthHash(word, seed)).start

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

  class HashApp(
    val fileReader: FileReader,
    val console: Console,
    val validator: Validator,
    val hash: Hash,
    val storage: Storage[HashItem]
  )(implicit sync: Sync[IO], cs: ContextShift[IO]) {
    private val storageKey: String = "min-hash"
    private def storeMinHash(hash: HashItem): IO[Unit]
      = storage.add(storageKey, hash)

    def run(): IO[ExitCode] = {
      (for {
        _ <- console.printLine("Enter file name:")
        fiber1 <- console.readLine.start
        dirtyName <- fiber1.join
        fileName <- IO.fromEither(validator.validateFileName(dirtyName))

        _ <- console.printLine("Enter seed:")
        fiber2 <- console.readLine.start
        dirtySeed <- fiber2.join
        seed <- IO.fromEither(validator.validateSeed(dirtySeed))

        dirtyData <- fileReader.readFile(fileName)
        data <- IO.fromEither(validator.validateContent(dirtyData))
        minHash <- hash.getMinHash(data, seed)

        _ <- storeMinHash(minHash)
      } yield ExitCode.Success)
        .handleErrorWith(e => {
          e match {
            case ValidationError.NotNumber
              => console.printLine(s"Error: Seed should be a number in range ${Int.MinValue} - ${Int.MaxValue}")
            case ValidationError.EmptyString => console.printLine("Error: Filename can't be empty")
            case ValidationError.EmptyFile   => console.printLine("Error: File is empty")
            case NonFatal(e)                 => console.printLine(s"Error: ${e.getMessage}")
          }

          console.printLine("-".repeat(33))
          run()
        })
    }
  }
}
