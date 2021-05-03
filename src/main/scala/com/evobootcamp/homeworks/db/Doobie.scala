package com.evobootcamp.homeworks.db

import cats.data.Validated

import scala.concurrent.ExecutionContext
import cats.effect.{Async, Blocker, ContextShift, ExitCode, IO, IOApp, Resource}
import cats.syntax.all._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import doobie._
import doobie.hikari.HikariTransactor
import doobie.implicits._
import doobie.implicits.javatime._
import doobie.h2._

import java.time.{LocalDate, Year}
import java.util.UUID
import scala.util.Try

object DbConfig {
  val dbDriverName = "org.h2.Driver"
  val dbUrl = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1"
  val dbUser = ""
  val dbPwd = ""
}

object Models {
  implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
  implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)

  final case class Author(id: UUID, name: String, birthday: LocalDate)

  final case class AuthorBook(id: UUID, title: String, year: Year, genre: String)

  final case class AuthorWithBooks(id: UUID, name: String, birthday: LocalDate, books: List[AuthorBook])

  final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: String)

  final case class BookWithAuthor(id: UUID, title: String, year: Year, genre: String, author: Author)
}

object Protocol {
  object UUIDParam {
    def unapply(arg: String): Option[UUID] = Try(UUID.fromString(arg)).toOption
  }

  implicit val yearDecoder: QueryParamDecoder[Year] = { param =>
    Validated
      .catchNonFatal(Year.parse(param.value))
      .leftMap(t => ParseFailure(s"Failed to decode year", t.getMessage))
      .toValidatedNel
  }

  implicit val uuidDecoder: QueryParamDecoder[UUID] = { param =>
    Validated
      .catchNonFatal(UUID.fromString(param.value))
      .leftMap(t => ParseFailure(s"Failed to decode UUID", t.getMessage))
      .toValidatedNel
  }

  object OptionalYearMatcher extends OptionalQueryParamDecoderMatcher[Year](name = "year")

  object OptionalAuthorMatcher extends OptionalQueryParamDecoderMatcher[UUID](name = "authorId")

  object OptionalNameMatcher extends OptionalQueryParamDecoderMatcher[String](name = "name")

  final case class CreateAuthor(name: String, birthday: LocalDate)

  final case class CreateBook(authorId: UUID, title: String, year: Year, genre: String)
}

object DbSetup {
  val authorOdersky: UUID = UUID.randomUUID()
  val authorRowling: UUID = UUID.randomUUID()
  val bookScala: UUID = UUID.randomUUID()
  val bookHPStone: UUID = UUID.randomUUID()
  val bookHPSecrets: UUID = UUID.randomUUID()

  val createTableAuthorsSql: String =
    """CREATE TABLE authors (
      |  id UUID PRIMARY KEY,
      |  name VARCHAR(100) NOT NULL,
      |  birthday DATE);""".stripMargin

  val createTableBooksSql: String =
    """CREATE TABLE books (
      |  id UUID PRIMARY KEY,
      |  author UUID NOT NULL,
      |  title VARCHAR(100) NOT NULL,
      |  year INT,
      |  genre VARCHAR(100) NOT NULL,
      |  FOREIGN KEY (author) REFERENCES authors(id) ON DELETE CASCADE);""".stripMargin

  val populateDataSql: String =
    s"""
       |INSERT INTO authors (id, name, birthday) VALUES
       |  ('$authorOdersky', 'Martin Odersky', '1958-09-05'),
       |  ('$authorRowling', 'J.K. Rowling', '1965-07-31');
       |
       |INSERT INTO books (id, author, title, year, genre) VALUES
       |  ('$bookScala', '$authorOdersky', 'Programming in Scala', 2016, 'Technical'),
       |  ('$bookHPStone', '$authorRowling', 'Harry Potter and Philosopher''s Stone', 1997, 'Fantastic'),
       |  ('$bookHPSecrets', '$authorRowling', 'Harry Potter and the Chamber of Secrets', 1998, 'Fantastic');
       |""".stripMargin

  val ddl1 = Fragment.const(createTableAuthorsSql)
  val ddl2 = Fragment.const(createTableBooksSql)
  val dml = Fragment.const(populateDataSql)

  def setup(): ConnectionIO[Unit] =
    for {
      _ <- ddl1.update.run
      _ <- ddl2.update.run
      _ <- dml.update.run
    } yield ()
}

object Db {
  import Models._

  object Connection {
    import DbConfig._

    def make[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
      Blocker[F].map { be =>
        Transactor.fromDriverManager[F](
          driver = dbDriverName,
          url = dbUrl,
          user = dbUser,
          pass = dbPwd,
          blocker = be,
        )
      }

    def pooled[F[_]: ContextShift: Async]: Resource[F, Transactor[F]] =
      for {
        ce <- ExecutionContexts.fixedThreadPool[F](4)
        be <- Blocker[F]
        xa <- HikariTransactor.newHikariTransactor[F](
          driverClassName = dbDriverName,
          url = dbUrl,
          user = dbUser,
          pass = dbPwd,
          connectEC = ce,
          blocker = be,
        )
      } yield xa
  }

  trait DbError
  object Errors {
    case object DatabaseError extends DbError
    case object AuthorNotFound extends DbError
    case object BookNotFound extends DbError
  }

  private val FOREIGN_KEY_VIOLATION = SqlState("23506")

  val authors: Fragment =
    fr"SELECT id, name, birthday FROM authors"

  val books: Fragment =
    fr"SELECT id, author, title, year, genre FROM books"

  def fetchAuthrors(): ConnectionIO[List[Author]] = {
    authors.query[Author].to[List]
  }

  def fetchBooks(): ConnectionIO[List[Book]] = {
    books.query[Book].to[List]
  }

  def insertAuthor(name: String, birthday: LocalDate): ConnectionIO[Either[DbError, Author]] = {
    sql"INSERT INTO authors (id, name, birthday) VALUES (${UUID.randomUUID()}, $name, $birthday)"
      .update
      .withGeneratedKeys[UUID]("id")
      .attemptSomeSqlState {
        case _ => Errors.DatabaseError.asInstanceOf[DbError]
      }
      .map(r => r.map(id => Author(id, name, birthday)))
      .compile
      .lastOrError
  }

  def updateAuthor(id: UUID, name: String, birthday: LocalDate): ConnectionIO[Either[DbError, Author]] = {
    sql"UPDATE authors SET name = $name, birthday = $birthday WHERE id = $id"
      .update
      .run
      .map(r => {
        if (r > 0) Right(Author(id, name, birthday)) else Left(Errors.AuthorNotFound)
      })
  }

  def findAuthors(name: Option[String]): ConnectionIO[List[Author]] = {
    (
      authors ++
      Fragments.whereAndOpt(
        name.map(n => fr"name ilike ${"%" + n + "%"}")
      )
    )
    .query[Author]
    .to[List]
  }

  def findAuthor(id: UUID): ConnectionIO[Either[DbError, AuthorWithBooks]] = {
    sql"""SELECT a.id, a.name, a.birthday, b.id, b.title, b.year, b.genre
         | FROM authors a
         | LEFT JOIN books b ON b.author = a.id
         | WHERE a.id = $id
       """
      .stripMargin
      .query[(Author, Option[AuthorBook])]
      .to[List]
      .map {
        case (author, book) :: xs => Right(
          xs.foldLeft[AuthorWithBooks](
            AuthorWithBooks(author.id, author.name, author.birthday, book.toList)
          ) {
            case (acc, (_, Some(book))) => acc.copy(books = book :: acc.books)
            case (acc, _) => acc
          }
        )
        case Nil => Left(Errors.AuthorNotFound)
      }
  }

  def deleteAuthor(id: UUID): ConnectionIO[Either[DbError, Unit]] = {
    sql"""DELETE FROM authors WHERE id = $id"""
      .update
      .run
      .map(res => if (res > 0) Right() else Left(Errors.AuthorNotFound))
  }

  def insertBook(authorId: UUID, title: String, year: Year, genre: String): ConnectionIO[Either[DbError, Book]] = {
    sql"""INSERT INTO books (id, author, title, year, genre)
         |VALUES (${UUID.randomUUID()}, $authorId, $title, $year, $genre)"""
      .stripMargin
      .update
      .withGeneratedKeys[UUID]("id")
      .attemptSomeSqlState {
        case FOREIGN_KEY_VIOLATION => Errors.AuthorNotFound.asInstanceOf[DbError]
        case _ => Errors.DatabaseError.asInstanceOf[DbError]
      }
      .map(r => r.map(id => Book(id, authorId, title, year, genre)))
      .compile
      .lastOrError
  }

  def updateBook(id: UUID, authorId: UUID, title: String, year: Year, genre: String): ConnectionIO[Either[DbError, Book]] = {
    sql"UPDATE books SET author = $authorId, title = $title, year = $year, genre = $genre WHERE id = $id"
      .update
      .run
      .attemptSomeSqlState {
        case FOREIGN_KEY_VIOLATION => Errors.AuthorNotFound.asInstanceOf[DbError]
        case _ => Errors.DatabaseError.asInstanceOf[DbError]
      }
      .map(res => {
        res.flatMap(r => if (r > 0) Right(Book(id, authorId, title, year, genre)) else Left(Errors.AuthorNotFound))
      })
  }

  def findBooks(authorId: Option[UUID], year: Option[Year]): ConnectionIO[List[Book]] = {
    (books ++ Fragments.whereAndOpt(
        authorId.map(id => fr"author = $id"),
        year.map(y => fr"year = $y")
    ))
    .query[Book]
    .to[List]
  }

  def findBook(id: UUID): ConnectionIO[Either[DbError, BookWithAuthor]] = {
    sql"""SELECT b.id, b.title, b.year, b.genre, a.id, a.name, a.birthday
         | FROM books b
         | JOIN authors a ON a.id = b.author
         | WHERE b.id = $id"""
      .stripMargin
      .query[BookWithAuthor]
      .option
      .map {
        case Some(b) => Right(b)
        case None    => Left(Errors.BookNotFound)
      }
  }

  def deleteBook(id: UUID): ConnectionIO[Either[DbError, Unit]] = {
    sql"""DELETE FROM books WHERE id = $id"""
      .update
      .run
      .map(res => if (res > 0) Right() else Left(Errors.BookNotFound))
  }

  def runQuery[F[_] : ContextShift : Async, A](query: ConnectionIO[A]): F[A] = {
    Connection.pooled[F].use { cn =>
      for {
        result <- query.transact(cn)
      } yield result
    }
  }
}

object Doobie extends IOApp {
  import Models._
  import Db._
  import Db.Errors._
  import Protocol._

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private case class BadResponse(message: String)

  private def getErrorMessage(e: DbError): String = e match {
    case DatabaseError  => "Database query error"
    case AuthorNotFound => "Author is not found"
    case BookNotFound   => "Book is not found"
    case _ => "Internal error"
  }

  private def renderResult[A](r: Either[DbError, A])(implicit ee: EntityEncoder[IO, A]): IO[Response[IO]] = {
    r match {
      case Left(e) => e match {
        case BookNotFound => NotFound(BadResponse(getErrorMessage(e)))
        case e => BadRequest(BadResponse(getErrorMessage(e)))
      }
      case Right(v) => Ok(v)
    }
  }

  private object CRUDController {
    def update[A, B](req: Request[IO], query: A => ConnectionIO[Either[DbError, B]])
    (implicit ed: EntityDecoder[IO, A], ee: EntityEncoder[IO, B]): IO[Response[IO]] = {
      req.as[A].flatMap { data =>
        for {
          item <- runQuery[IO, Either[DbError, B]](query(data))
          result <- renderResult[B](item)
        } yield result
      }
    }

    def delete(query: ConnectionIO[Either[DbError, Unit]]): IO[Response[IO]] = {
      for {
        res <- runQuery[IO, Either[DbError, Unit]](query)
        result <- renderResult[Unit](res)
      } yield result
    }

    def findOne[B](query: ConnectionIO[Either[DbError, B]])
    (implicit ee: EntityEncoder[IO, B]): IO[Response[IO]] = {
      for {
        item <- runQuery[IO, Either[DbError, B]](query)
        result <- renderResult[B](item)
      } yield result
    }

    def findAll[B](query: ConnectionIO[List[B]])
               (implicit ee: EntityEncoder[IO, List[B]]): IO[Response[IO]] = {
      for {
        items <- runQuery[IO, List[B]](query).handleErrorWith(e => {
          println(e)
          IO.pure(List())
        })
        result <- Ok(items)
      } yield result
    }
  }

  private val bookRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "books" :? OptionalYearMatcher(year) +& OptionalAuthorMatcher(authorId) =>
      CRUDController.findAll(findBooks(authorId, year))

    case GET -> Root / "book" / UUIDParam(id) =>
      CRUDController.findOne[BookWithAuthor](findBook(id))

    case req @ POST -> Root / "books" =>
      CRUDController.update[CreateBook, Book](
        req,
        data => insertBook(data.authorId, data.title, data.year, data.genre)
      )

    case req @ PUT -> Root / "book" / UUIDParam(id) =>
      CRUDController.update[CreateBook, Book](
        req,
        data => updateBook(id, data.authorId, data.title, data.year, data.genre)
      )

    case DELETE -> Root / "book" / UUIDParam(id) =>
      CRUDController.delete(deleteBook(id))
  }

  private val authorRoutes = HttpRoutes.of[IO] {
    case GET -> Root / "authors" :? OptionalNameMatcher(name) =>
      CRUDController.findAll(findAuthors(name))

    case GET -> Root / "author" / UUIDParam(id) =>
      CRUDController.findOne[AuthorWithBooks](findAuthor(id))

    case req @ POST -> Root / "authors" =>
      CRUDController.update[CreateAuthor, Author](
        req,
        createAuthor => insertAuthor(createAuthor.name, createAuthor.birthday)
      )

    case req @ PUT -> Root / "author" / UUIDParam(id) =>
      CRUDController.update[CreateAuthor, Author](
        req,
        data => updateAuthor(id, data.name, data.birthday)
      )

    case DELETE -> Root / "author" / UUIDParam(id) =>
      CRUDController.delete(deleteAuthor(id))
  }

  private val httpApp = (bookRoutes <+> authorRoutes).orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    runQuery[IO, Unit](DbSetup.setup).flatMap { _ =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(port = 9876, host = "localhost")
        .withHttpApp(httpApp)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
  }
}
