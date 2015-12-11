package model.commercial.jobs

import java.lang.System.currentTimeMillis
import java.util.concurrent.TimeUnit.MILLISECONDS

import commercial.feeds.{MissingFeedException, ParsedFeed}
import common.{ExecutionContexts, Logging}
import conf.Configuration.commercial.merchandisingFeedsRoot
import services.S3

import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}
import scala.xml.{Elem, XML}

// todo switch off
// todo unparseable
object JobsFeed extends ExecutionContexts with Logging {

  def parse(xml: Elem): Seq[Job] = for {
    jobXml <- xml \\ "Job"
    if (jobXml \ "RecruiterLogoURL").nonEmpty
    if (jobXml \ "RecruiterName").text != "THE GUARDIAN MASTERCLASSES"
  } yield Job(jobXml)

  def parsedJobs(feedName:String): Try[ParsedFeed] = {
    val start = currentTimeMillis
    S3.get(s"$merchandisingFeedsRoot/$feedName") map { body =>
      val parsed = parse(XML.loadString(body.dropWhile(_ != '<')))
      Try(ParsedFeed(parsed, Duration(currentTimeMillis - start, MILLISECONDS)))
    } getOrElse {
      Failure(MissingFeedException)
    }
  }
}
