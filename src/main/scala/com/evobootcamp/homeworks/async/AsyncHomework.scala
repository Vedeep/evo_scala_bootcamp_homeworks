package com.evobootcamp.homeworks.async

import cats.implicits._

import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  //put your code there
  Await
    .ready(run(args.toList), Duration.Inf)
    .foreach(names => {
      println("Server names:")
      names.foreach(println)
    })

  private def run(urls: List[String]): Future[List[String]] =
    for {
      bodies <- urls.traverse(fetchPageBody)
      links <- bodies.traverse(findLinkUrls)
      serverNames <- links.flatten.toSet.toList.traverse(fetchServerName)
      names <- Future.successful(serverNames.toSet.flatMap[String](s => s).toList.sorted)
    } yield names

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      println(s"${Thread.currentThread().getName} Ready $url")
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {

      val a = Option(new URL(url).openConnection().getHeaderField("Server"))
      println(s"${Thread.currentThread().getName} Ready $url")
      a
    }
  }


  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }
}
