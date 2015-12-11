package model.commercial.jobs

import commercial.feeds.ParsedFeed
import common.ExecutionContexts
import model.commercial._

import scala.concurrent.Future
import scala.util.Try

object JobsAgent extends MerchandiseAgent[Job] with ExecutionContexts {

  def jobsTargetedAt(segment: Segment): Seq[Job] = {
    def defaultJobs = available filter (_.industries.contains("Media"))
    getTargetedMerchandise(segment, defaultJobs) { job =>
      Keyword.idSuffixesIntersect(segment.context.keywords, job.keywordIdSuffixes)
    }
  }

  def specificJobs(jobIdStrings: Seq[String]): Seq[Job] = {
    val jobIds = jobIdStrings map (_.toInt)
    available filter (job => jobIds contains job.id)
  }

  def refresh(feedName: String): Future[ParsedFeed] = {

    def withKeywords(parsedFeed: Try[ParsedFeed]): Try[ParsedFeed] = {
      parsedFeed map { feed =>
        val jobs = feed.jobs map { job =>
          val jobKeywordIds = job.sectorIds.flatMap(Industries.forIndustry).distinct
          job.copy(keywordIdSuffixes = jobKeywordIds map Keyword.getIdSuffix)
        }
        ParsedFeed(jobs, feed.parseDuration)
      }
    }

    val parsedFeed = withKeywords(JobsFeed.parsedJobs(feedName))

    parsedFeed map { feed =>
      updateAvailableMerchandise(feed.jobs) map {
        ParsedFeed(_, feed.parseDuration)
      }
    } getOrElse {
      Future.fromTry(parsedFeed)
    }
  }
}
