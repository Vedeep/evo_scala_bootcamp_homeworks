package com.evobootcamp.homeworks.http

import io.circe.{Decoder, HCursor, Json}
import cats.effect.{Blocker, ExitCode, IO, IOApp, Sync}
import cats.data.EitherT
import cats.syntax.all._
import fs2.Pipe
import fs2.concurrent.Queue
import io.circe.Decoder.Result
import io.circe.generic.JsonCodec
import io.circe.generic.semiauto.deriveDecoder

import scala.annotation.tailrec
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import scala.concurrent.ExecutionContext

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// The exact protocol and message format to use is not specified and should be designed while working on the task.

object Router {
  type RouteFind[F[+_], +A, +E] = PartialFunction[Request[F], Option[RouteAction[F, A, E]]]
  type RouteAction[F[+_], +A, +E] = Request[F] => F[Either[E, A]]

  trait Route[F[+_], +A, +E] {
    def check: RouteFind[F, A, E]
  }

  trait Router[F[+_], +A, +E] {
    def find(req: Request[F]): Option[RouteAction[F, A, E]]
  }

  trait RouteRoot {
    val path: Path
  }

  final class GameRouter[F[+_], +A, +E] private (val routes: List[Route[F, A, E]]) extends Router[F, A, E] {
    override def find(req: Request[F]): Option[RouteAction[F, A, E]] = {
      @tailrec
      def recursive(routes: List[Route[F, A, E]]): Option[RouteAction[F, A, E]] = {
        routes match {
          case route :: tail => {
            route.check
            .applyOrElse[Request[F], Option[RouteAction[F, A, E]]](req, _ => None) match {
              case act @ Some(_) => act
              case _ => recursive(tail)
            }
          }
          case _ => None
        }
      }

      recursive(routes)
    }
  }

  final class GameRoute[F[+_], +A, +E](checkF: RouteFind[F, A, E]) extends Route[F, A, E] {
    override def check: RouteFind[F, A, E] = checkF
  }

  object GameRoute {
    def apply[F[+_], A, E](checkF: RouteFind[F, A, E]): GameRoute[F, A, E]
      = new GameRoute(checkF)
  }

  object GameRouter {
    def ofRoutes[F[+_], A, E](routes: List[Route[F, A, E]]): Router[F, A, E] = new GameRouter[F, A, E](routes)
  }

  sealed case class GameRouteRoot(path: Path) extends RouteRoot
}

object WebSocketRouter {
  import Router._

  final case class IncomingMessage(command: String)

  trait WsRouter[F[+_], +A, +E] {
    def find(command: String): Option[RouteAction[F, A, E]]
  }

  trait WsRouteAction[F[+_], +A, +E] {
    def execute(msg: String): F[Either[E, A]]
  }

//  implicit val incMessageDecoder: Decoder[IncomingMessage] = deriveDecoder[IncomingMessage]


//
//  case class TestParams(min: Long, max: Long)
//
//  case class Test(command: String = "get-token", params: TestParams) extends IncomingMessage[TestParams]
}

object Games {
  import Router._

  trait GameError extends Error
  object GameErrors {
    case object WrongParams extends GameError
    case object NumberMinMoreMaxError extends GameError
    case object NumberMinEqualsMaxError extends GameError
    case object TokenNotFound extends GameError
    case object InvalidToken extends GameError
    case object ValueLessThanResult extends GameError
    case object ValueMoreThanResult extends GameError
    case object GameNotStarted extends GameError
  }

  trait Game {}

  trait GameActionResponse extends Product with Serializable {}

  trait GameRoutes {
    def routesOf[F[+_]](root: Path): List[Route[F, GameActionResponse, GameError]]
  }

  final case class TheGuessStartParams(min: Long, max: Long) {}
  final case class TheGuessStartResult(min: Long, max: Long, result: Long) extends GameActionResponse {}

  final case class TheGuessPickParams(num: Long) {}
  final case class TheGuessPickResult(result: Long) extends GameActionResponse {}

  final class TheGuess private extends Game {
    def start[F[_]](params: TheGuessStartParams)(implicit sf: Sync[F]): F[Either[GameError, TheGuessStartResult]] = {
      Sync[F].pure(params match {
        case TheGuessStartParams(min, max) if min > max => Left(GameErrors.NumberMinMoreMaxError)
        case TheGuessStartParams(min, max) if min == max => Left(GameErrors.NumberMinEqualsMaxError)
        case TheGuessStartParams(min, max) => Right(TheGuessStartResult(min ,max, getRandomNumber(min, max)))
        case _ => Left(GameErrors.WrongParams)
      })
    }

    def pick[F[_]](params: TheGuessPickParams, result: Long)
    (implicit sf: Sync[F]): F[Either[GameError, TheGuessPickResult]] = {
      Sync[F].pure(
        if (params.num > result) Left(GameErrors.ValueMoreThanResult)
        else if (params.num < result) Left(GameErrors.ValueLessThanResult)
        else Right(TheGuessPickResult(result))
      )
    }

    private def getRandomNumber(min: Long, max: Long): Long = scala.util.Random.between(min, max)
  }

  object TheGuess {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    def routesOf[F[+_]](root: RouteRoot)(implicit s: Sync[F]): List[GameRoute[F, GameActionResponse, GameError]] = {
      GameRoute[F, GameActionResponse, GameError](
        {
          case POST -> root.path / "start" => Some(start)
        }
      ) ::
      GameRoute[F, GameActionResponse, GameError](
        {
          case POST -> root.path / "pick"  => Some(pick)
        }
      ) :: Nil
    }

    def apply: TheGuess = new TheGuess

    private def start[F[+_]](implicit s: Sync[F]): RouteAction[F, TheGuessStartResult, GameError] = (req: Request[F]) => {
      req.as[TheGuessStartParams].flatMap { params =>
        apply.start(params)
      }
    }

    private def pick[F[+_]](implicit s: Sync[F]): RouteAction[F, TheGuessPickResult, GameError] = (req: Request[F]) => {
      req.as[TheGuessPickParams].flatMap { params =>
        (for {
          rightAnswer <- EitherT.fromOption[F](
            req.cookies.find(_.name == "guess-result").flatMap(_.content.toLongOption),
            GameErrors.GameNotStarted
          )
          result <- EitherT(apply.pick(params, rightAnswer))
        } yield result).value
      }
    }
  }

}

object GuessServer extends IOApp {
  import Router._
  import Games._

  import org.http4s.circe.CirceEntityCodec._

  private val ApiRoot = Root / "api" / "v1"
  private val GuessRoot = GameRouteRoot(ApiRoot / "the-guess")

  object Formatter {
    private def errorFromString(e: String): Json = {
      Json.obj(
        "result" -> Json.fromString("error"),
        "message" -> Json.fromString(e)
      )
    }

    def formatGameError(e: GameError): IO[Response[IO]] = e match {
      case GameErrors.WrongParams => BadRequest(errorFromString("Wrong incoming params"))
      case GameErrors.NumberMinEqualsMaxError => BadRequest(errorFromString("Min value can't be equals max value"))
      case GameErrors.NumberMinMoreMaxError => BadRequest(errorFromString("Min value can't be more then max value"))
      case GameErrors.TokenNotFound => BadRequest(errorFromString("Please provide access token"))
      case GameErrors.InvalidToken => BadRequest(errorFromString("Token is invalid"))
      case GameErrors.ValueLessThanResult => BadRequest(errorFromString("Value less than result"))
      case GameErrors.ValueMoreThanResult => BadRequest(errorFromString("Value more than result"))
      case GameErrors.GameNotStarted => BadRequest(errorFromString("Game not started"))
      case _ => InternalServerError(errorFromString("Unsupported error type"))
    }

    def formatGameResult(r: GameActionResponse): IO[Response[IO]] = r match {
      case TheGuessStartResult(min, max, result) => Ok(
        Json.obj(
          "result" -> Json.fromString("OK"),
          "message" -> Json.fromString(s"Game started with params $min - $max")
        )
      ).map(_.addCookie("guess-result", result.toString))
      case TheGuessPickResult(res) => Ok(Json.obj(
        "result" -> Json.fromString("OK"),
        "message" -> Json.fromString(s"Game is ended, result = $res.")
      )).map(_.removeCookie("guess-result"))
      case _ => Ok(Json.Null)
    }
  }

  private val gamesRoutes = {
    val router = GameRouter.ofRoutes[IO, GameActionResponse, GameError](
      TheGuess.routesOf[IO](GuessRoot)
    )

    HttpRoutes.of[IO] { req =>
      router.find(req) match {
        case Some(route) => route(req).flatMap {
          case Left(e)  => Formatter.formatGameError(e)
          case Right(r) => Formatter.formatGameResult(r)
        }
      }
    }
  }

  import WebSocketRouter._

  private val wsRoutes = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._
    import io.circe.syntax._

//    val router =

    HttpRoutes.of[IO] {
      case GET -> Root / "ws" / "v1" / "games" => {
        val pipe: Pipe[IO, WebSocketFrame, WebSocketFrame] = _.collect {
          case WebSocketFrame.Text(message, _) => {
            (for {
              msg <- io.circe.jawn.decode[IncomingMessage](message)
              _ <- Right()
            } yield msg) match {
              case Right(msg) => WebSocketFrame.Text("Vse zbs")
              case Left(e) => WebSocketFrame.Text(e.toString)
            }


//            WebSocketFrame.Text(message)
          }
        }

        for {
          queue <- Queue.unbounded[IO, WebSocketFrame]
          response <- WebSocketBuilder[IO].build(
            receive = queue.enqueue,
            send = queue.dequeue.through(pipe),
          )
        } yield response
      }
    }
  }

  private val httpApp = (gamesRoutes <+> wsRoutes).orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9876, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}

object GuessClient extends IOApp {
  private val uri = uri"http://localhost:9876/api/v1/the-guess"
  private val min = 0
  private val max = 100

  case class GuessStartRequest(min: Long, max: Long)
  case class GuessPickRequest(num: Long)
  case class GuessResponse(result: String, message: String)

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private def pick(client: Client[IO], cookie: ResponseCookie, num: Long): IO[GuessResponse] = {
    def recursive(num: Long): IO[GuessResponse] = {
      client.expect[GuessResponse](
        Method.POST(GuessPickRequest(num), uri / "pick").map(_.addCookie(cookie.name, cookie.content))
      ).handleErrorWith(_ => {
        recursive(num + 1)
      })
    }

    recursive(num)
  }

  private def start(client: Client[IO], min: Long, max: Long): IO[Either[Throwable, ResponseCookie]] = {
    Method.POST(
      GuessStartRequest(min, max),
      uri / "start"
    ).flatMap[Either[Throwable, ResponseCookie]](client.run(_).use { resp: Response[IO] =>
      if (resp.status == Status.Ok) {
        IO.pure(resp.cookies.find(_.name == "guess-result").toRight(new Error("Not found")))
      } else {
        resp.bodyText
          .map(e => {
            Left(new Error(e))
          }).compile.lastOrError
      }
    })
  }

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global)
      .resource
      .parZip(Blocker[IO]).use { case (client, _) =>
      for {
        startResult <- start(client, min ,max)
        cookie <- IO.fromEither(startResult)
        pickResponse <- pick(client, cookie, min)
          _ <- IO.pure(println(pickResponse.message))
      } yield ()
    }.as(ExitCode.Success)
  }
}
