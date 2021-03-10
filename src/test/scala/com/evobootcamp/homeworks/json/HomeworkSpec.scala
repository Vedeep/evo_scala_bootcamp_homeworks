package com.evobootcamp.homeworks.json

import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.{LocalDate, ZonedDateTime}
import cats.instances.either._
import cats.syntax.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.Decoder
import io.circe.parser._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredDecoder
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

import java.time.temporal.ChronoField
import scala.io.Source
import scala.util.Try

/**
 * HOMEWORK:
 *
 * Some classes and generated JSON codecs are provided for NBA API.
 * Unfortunately, they don't work as expected out of the box.
 * The task is to fix (rewrite) some of the codecs to make tests pass.
 * You are not supposed to change anything in _class_ HomeworkSpec,
 * instead of it you are supposed to keep your changes inside _companion object_ for HomeworkSpec.
 *
 * You are not allowed to rename fields in case classes.
 * You are not allowed to remove fields from case classes.
 * You are supposed to use camelCase if you introduce additional fields in case classes.
 *
 * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
 */
class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      gameInfos.size must be(1)
    }
  }

}

object HomeworkSpec {
  final case class TeamTotals(assists: String, fullTimeoutRemaining: String, plusMinus: String)
  final case class TeamBoxScore(totals: TeamTotals)
  final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)
  final case class PrevMatchup(gameDate: LocalDate, gameId: String)
  final case class BoxScore(
                                        basicGameData: Game,
                                        previousMatchup: PrevMatchup,
                                        stats: Option[GameStats],
                                      )
  final case class JustScore(score: String)
  final case class TeamStats(
                                         linescore: List[JustScore],
                                         loss: String,
                                         score: String,
                                         teamId: String,
                                         triCode: String
                                       )
  final case class GameDuration(hours: String, minutes: String)
  final case class Arena(
                                     city: String,
                                     country: String,
                                     isDomestic: Boolean,
                                     name: String,
                                     stateAbbr: String
                                   )
  final case class Game(
                                    arena: Arena,
                                    attendance: String,
                                    endTimeUTC: Option[ZonedDateTime],
                                    gameDuration: GameDuration,
                                    gameId: String,
                                    gameUrlCode: String,
                                    hTeam: TeamStats,
                                    isBuzzerBeater: Boolean,
                                    startTimeUTC: ZonedDateTime,
                                    vTeam: TeamStats,
                                  )
  final case class Scoreboard(games: List[Game], numGames: Int)

  object ModelsJson {
    implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames
      .copy(transformMemberNames = {
        case "fullTimeoutRemaining" => "full_timeout_remaining"
        case name => name
      })

    val localDateFormat = new DateTimeFormatterBuilder()
      .appendValue(ChronoField.YEAR, 4)
      .appendValue(ChronoField.MONTH_OF_YEAR, 2)
      .appendValue(ChronoField.DAY_OF_MONTH, 2)
      .toFormatter()

    implicit val localDateDecoder: Decoder[LocalDate] = Decoder.decodeString.emap { date =>
      Either.catchNonFatal(LocalDate.parse(date, localDateFormat)).leftMap(err => err.getMessage)
    }

    implicit val teamTotalsDecoder: Decoder[TeamTotals] = deriveConfiguredDecoder[TeamTotals]

    implicit val teamBoxScoreDecoder: Decoder[TeamBoxScore] = deriveConfiguredDecoder[TeamBoxScore]

    implicit val gameStatsDecoder: Decoder[GameStats] = deriveConfiguredDecoder[GameStats]

    implicit val prevMatchupDecoder: Decoder[PrevMatchup] = deriveConfiguredDecoder[PrevMatchup]

    implicit val boxScoreDecoder: Decoder[BoxScore] = deriveConfiguredDecoder[BoxScore]

    implicit val justScoreDecoder: Decoder[JustScore] = deriveConfiguredDecoder[JustScore]

    implicit val teamStatsDecoder: Decoder[TeamStats] = deriveConfiguredDecoder[TeamStats]

    implicit val gameDurationDecoder: Decoder[GameDuration] = deriveConfiguredDecoder[GameDuration]

    implicit val arenaDecoder: Decoder[Arena] = deriveConfiguredDecoder[Arena]

    implicit val gameDecoder: Decoder[Game] = deriveConfiguredDecoder[Game]

    implicit val scoreboardDecoder: Decoder[Scoreboard] = deriveConfiguredDecoder[Scoreboard]
  }

  import ModelsJson._

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val jsonString = Try {
      val src = Source.fromResource(s"scoreboard_$dateString.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json"
      Http(url).asString.body
    }
    decode[Scoreboard](jsonString)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val jsonString = Try {
      val src = Source.fromResource(s"${gameId}_boxscore.json")
      val s = src.mkString
      src.close()
      s
    }.getOrElse {
      val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
      val url = s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json"
      Http(url).asString.body
    }
    decode[BoxScore](jsonString)
  }
}
