package com.evobootcamp.homeworks.effects

import cats.effect._
import cats.effect.concurrent._
import cats.effect.testing.scalatest.AsyncIOSpec
import com.evobootcamp.homeworks.effects.SharedStateHomework._
import org.scalatest.flatspec.{AsyncFlatSpec}
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.{DurationInt}

class SharedStateHomeworkSpec extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "Cache" should "add and return items" in {
    val state = Ref.unsafe[IO, Map[String, (Long, Int)]](Map.empty)
    val cache = new RefCache[IO, String, Int](state, 10.seconds)

    (for {
      _ <- cache.put("test1", 123)
      _ <- cache.put("test2", 456)
      _ <- cache.put("test3", 789)

      _ <- cache.get("test1").asserting(_ shouldBe Some(123))
      _ <- cache.get("test2").asserting(_ shouldBe Some(456))
      _ <- cache.get("test3").asserting(_ shouldBe Some(789))
    } yield ExitCode.Success).asserting(_ shouldBe ExitCode.Success)
  }

  "Cache" should "delete items if they have expired" in {
    (for {
      cache <- Cache.of[IO, String, Int](3.seconds, 1.seconds)
      _ <- cache.put("test1", 123)
      _ <- cache.put("test2", 456)
      _ <- cache.put("test3", 789)

      _ <- IO.sleep(2.seconds)

      _ <- cache.get("test1").asserting(_ shouldBe Some(123))
      _ <- cache.get("test2").asserting(_ shouldBe Some(456))
      _ <- cache.get("test3").asserting(_ shouldBe Some(789))

      _ <- IO.sleep(2.seconds)

      _ <- cache.get("test1").asserting(_ shouldBe None)
      _ <- cache.get("test2").asserting(_ shouldBe None)
      _ <- cache.get("test3").asserting(_ shouldBe None)
    } yield ExitCode.Success).asserting(_ shouldBe ExitCode.Success)
  }
}
