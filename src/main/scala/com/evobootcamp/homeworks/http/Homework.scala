package com.evobootcamp.homeworks.http

import io.circe.{Json}
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource, Sync}
import cats._
import cats.data.EitherT
import cats.syntax.all._
import fs2.Pipe
import fs2.concurrent.Queue

import scala.annotation.tailrec
import org.http4s._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame

import java.net.http.HttpClient
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

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
  type WsRouteFind[F[+_], +A, +E] = String => Option[WsRouteAction[F, A, E]]
  type WsRouteExecute[F[+_], +A, +E] = (String) => (WsSession[F]) => F[Either[E, A]]

  final case class IncomingMessage(command: String)

  trait IncomingMessageWithParams[A] {
    def command: String
    def params: A
  }

  trait WsRouter[F[+_], +A, +E] {
    def find(command: String): Option[WsRouteAction[F, A, E]]
  }

  trait WsRoute[F[+_], +A, +E] {
    def check: WsRouteFind[F, A, E]
  }

  trait WsRouteAction[F[+_], +A, +E] {
    def execute: WsRouteExecute[F, A, E]
  }

  trait WsSession[F[_]] {
    def get[A](key: String): F[Option[A]]
    def set[A](key: String, value: A): F[Unit]
    def del(key: String): F[Unit]
  }

  final class WsGameRouter[F[+_], +A, +E] private (val routes: List[WsRoute[F, A, E]]) extends WsRouter[F, A, E] {
    override def find(command: String): Option[WsRouteAction[F, A, E]] = {
      @tailrec
      def recursive(routes: List[WsRoute[F, A, E]]): Option[WsRouteAction[F, A, E]] = {
        routes match {
          case route :: tail => {
            route.check(command) match {
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

  final class WsGameRoute[F[+_], +A, +E](checkF: WsRouteFind[F, A, E]) extends WsRoute[F, A, E] {
    override def check = checkF
  }

  object WsGameRoute {
    def apply[F[+_], A, E](checkF: WsRouteFind[F, A, E]): WsGameRoute[F, A, E]
    = new WsGameRoute(checkF)
  }

  object WsGameRouter {
    def ofRoutes[F[+_], A, E](routes: List[WsRoute[F, A, E]]): WsRouter[F, A, E] = new WsGameRouter[F, A, E](routes)
  }

  sealed class GamesWsSession[F[_]](implicit sync: Sync[F]) extends WsSession[F] {
    private lazy val sessionMap: Ref[F, Map[String, Any]] = Ref.unsafe(Map.empty)
    private val session: F[Ref[F, Map[String, Any]]] = Sync[F].delay(sessionMap)

    override def set[A](key: String, value: A): F[Unit] = for {
      ref <- session
      _ <- ref.getAndUpdate(_ + (key -> value))
    } yield ()

    override def get[A](key: String): F[Option[A]] = for {
      ref <- session
      mp <- ref.get
    } yield mp.get(key).asInstanceOf[Option[A]]

    override def del(key: String): F[Unit] = for {
      ref <- session
      _ <- ref.getAndUpdate(_ - key)
    } yield ()
  }

  object GamesWsSession {
    def of[F[_]](implicit SF: Sync[F]): F[WsSession[F]] = Sync[F].pure(new GamesWsSession[F])
  }
}

object Games {
  import Router._
  import WebSocketRouter._

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
    case object InternalError extends GameError
    case object GameNotFound extends GameError
  }

  trait Game {}

  trait GameActionResponse extends Product with Serializable {}

  trait GameRoutes {
    def routesOf[F[+_]](root: Path): List[Route[F, GameActionResponse, GameError]]
  }

  final case class TheGuessStartParams(min: Long, max: Long) {}
  final case class TheGuessStartResult(min: Long, max: Long, result: Long) extends GameActionResponse {}
  final case class TheGuessStartParamsWs(command: String, params: TheGuessStartParams)
    extends IncomingMessageWithParams[TheGuessStartParams]

  final case class TheGuessPickParams(num: Long) {}
  final case class TheGuessPickResult(result: Long) extends GameActionResponse {}
  final case class TheGuessPickParamsWs(command: String, params: TheGuessPickParams)
    extends IncomingMessageWithParams[TheGuessPickParams]

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

    def wsRoutesOf[F[+_]](implicit mt: MonadThrow[F], s: Sync[F]): List[WsRoute[F, GameActionResponse, GameError]] = {
      WsGameRoute[F, GameActionResponse, GameError]({ command =>
        Option.when(command == "guess/start")(new TheGuessWsController.Start[F])
      }) ::
      WsGameRoute[F, GameActionResponse, GameError]({ command =>
        Option.when(command == "guess/pick")(new TheGuessWsController.Pick[F])
      }) :: Nil
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

    object TheGuessWsController {
      import io.circe.generic.auto._

      private val RESULT_KEY = "guess-result"

      class Start[F[+_]](implicit mt: MonadThrow[F], s: Sync[F]) extends WsRouteAction[F, TheGuessStartResult, GameError] {
        override def execute: WsRouteExecute[F, TheGuessStartResult, GameError]
        = (msg: String) => (session: WsSession[F]) => {
          (for {
            parsed <- EitherT.fromEither[F](io.circe.jawn.decode[TheGuessStartParamsWs](msg)
              .leftMap[GameError](_ => GameErrors.WrongParams))
            result <- EitherT(apply.start(parsed.params))
            _ <- EitherT.right[GameError](session.set[Long](RESULT_KEY, result.result))
          } yield result).value
        }
      }

      class Pick[F[+_]](implicit mt: MonadThrow[F], s: Sync[F]) extends WsRouteAction[F, TheGuessPickResult, GameError] {
        override def execute: WsRouteExecute[F, TheGuessPickResult, GameError]
        = (msg: String) => (session: WsSession[F]) => {
          (for {
            rightAnswer <- EitherT.fromOptionF(session.get[Long](RESULT_KEY), GameErrors.GameNotStarted)
            parsed      <- EitherT.fromEither[F](io.circe.jawn.decode[TheGuessPickParamsWs](msg)
              .leftMap[GameError](_ => GameErrors.WrongParams))
            result      <- EitherT(apply.pick(parsed.params, rightAnswer))
          } yield result).value
        }
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

    def getErrorMessage(e: GameError): Json = e match {
      case GameErrors.WrongParams => errorFromString("Wrong incoming params")
      case GameErrors.NumberMinEqualsMaxError => errorFromString("Min value can't be equals max value")
      case GameErrors.NumberMinMoreMaxError => errorFromString("Min value can't be more then max value")
      case GameErrors.TokenNotFound => errorFromString("Please provide access token")
      case GameErrors.InvalidToken => errorFromString("Token is invalid")
      case GameErrors.ValueLessThanResult => errorFromString("Value less than result")
      case GameErrors.ValueMoreThanResult => errorFromString("Value more than result")
      case GameErrors.GameNotStarted => errorFromString("Game not started")
      case GameErrors.InternalError => errorFromString("Internal error")
      case GameErrors.GameNotFound => errorFromString("Game not found")
      case _ => errorFromString("Unsupported error type")
    }

    def getResultMessage(r: GameActionResponse): Json = r match {
      case TheGuessStartResult(min, max, result) => Json.obj(
        "result" -> Json.fromString("OK"),
        "message" -> Json.fromString(s"Game started with params $min - $max")
      )
      case TheGuessPickResult(res) => Json.obj(
        "result" -> Json.fromString("OK"),
        "message" -> Json.fromString(s"Game is ended, result = $res.")
      )
      case _ => Json.Null
    }

    def formatGameError(e: GameError): IO[Response[IO]] = BadRequest(getErrorMessage(e))

    def formatGameResult(r: GameActionResponse): IO[Response[IO]] = r match {
      case TheGuessStartResult(_, _, result)
        => Ok(getResultMessage(r)).map(_.addCookie("guess-result", result.toString))
      case TheGuessPickResult(_)
        => Ok(getResultMessage(r)).map(_.removeCookie("guess-result"))
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

    val router = WsGameRouter.ofRoutes[IO, GameActionResponse, GameError](
      TheGuess.wsRoutesOf[IO]
    )

    HttpRoutes.of[IO] {
      case GET -> Root / "ws" / "v1" / "games" => {
        val userSession = GamesWsSession.of[IO]

        val pipe: Pipe[IO, WebSocketFrame, WebSocketFrame] = _.collect {
          case WebSocketFrame.Text(message, _) => {
            (for {
              incomingMessage <- EitherT.fromEither[IO](io.circe.jawn.decode[IncomingMessage](message)
                .leftMap(_ => GameErrors.WrongParams))
              route <- EitherT.fromOption[IO](
                router.find(incomingMessage.command),
                GameErrors.GameNotFound.asInstanceOf[GameError]
              )
              session <- EitherT.right(userSession)
              result <- EitherT(route.execute(message)(session))
            } yield result).value.unsafeRunSync() match {
              case Left(e) => WebSocketFrame.Text(Formatter.getErrorMessage(e).noSpaces)
              case Right(r) => WebSocketFrame.Text(Formatter.getResultMessage(r).noSpaces)
            }
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

object GuessClientWS extends IOApp {
  private val uri = uri"ws://localhost:9876/ws/v1/games"
  private val min = 0
  private val max = 100

  case class GuessRequest[A](command: String, params: A)
  case class GuessStartRequest(min: Long, max: Long)
  case class GuessPickRequest(num: Long)
  case class GuessResponse(result: String, message: String)

  import io.circe.generic.auto._
  import io.circe.syntax._

  private def pick(client: WSConnectionHighLevel[IO], num: Long): IO[GuessResponse] = {
    def recursive(num: Long): IO[GuessResponse] = {
      val result = for {
        _ <- client.send(WSFrame.Text(GuessRequest("guess/pick", GuessPickRequest(num)).asJson.noSpaces))
        msg <- client.receiveStream.collectFirst {
          case WSFrame.Text(s, _) => s
        }.compile.string
        resp <- IO.fromEither(io.circe.jawn.decode[GuessResponse](msg))
      } yield resp

      result.flatMap { resp =>
        if (resp.result == "OK") IO.pure(resp) else recursive(num + 1)
      }
    }

    recursive(num)
  }

  private def start(client: WSConnectionHighLevel[IO], min: Long, max: Long): IO[GuessResponse] = {
    for {
      _ <- client.send(WSFrame.Text(GuessRequest("guess/start", GuessStartRequest(min, max)).asJson.noSpaces))
      msg <- client.receiveStream.collectFirst {
        case WSFrame.Text(s, _) => s
      }.compile.string
      resp <- IO.fromEither(io.circe.jawn.decode[GuessResponse](msg))
    } yield resp
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val clientResource = Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))

    clientResource.use { client =>
      for {
        startResponse <- start(client, min, max)
        _ <- IO.raiseWhen(startResponse.result != "OK")(new Error(startResponse.message))
        _ <- IO.pure(println(startResponse.message))
        pickResponse <- pick(client, min)
        _ <- IO.pure(println(pickResponse.message))
      } yield ExitCode.Success
    }
  }
}
