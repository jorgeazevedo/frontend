package commercial

import commercial.feeds._
import common.{AkkaAsync, ExecutionContexts, Jobs, Logging}
import conf.Configuration.commercial.merchandisingFeedsRoot
import model.commercial.books.BestsellersAgent
import model.commercial.jobs.{Industries, JobsAgent}
import model.commercial.masterclasses.{MasterClassAgent, MasterClassTagsAgent}
import model.commercial.money.BestBuysAgent
import model.commercial.soulmates._
import model.commercial.travel.{Countries, TravelOffersAgent}
import model.diagnostics.CloudWatch
import play.api.{Application => PlayApp, GlobalSettings}
import services.S3

import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

trait CommercialLifecycle extends GlobalSettings with Logging with ExecutionContexts {

  private val feedFetchers: Seq[FeedFetcher] = Seq(
    FeedFetcher.jobs,
    FeedFetcher.soulmates(MaleSoulmatesFeed),
    FeedFetcher.soulmates(NewMenSoulmatesFeed),
    FeedFetcher.soulmates(FemaleSoulmatesFeed),
    FeedFetcher.soulmates(NewWomenSoulmatesFeed),
    FeedFetcher.soulmates(BrightonSoulmatesFeed),
    FeedFetcher.soulmates(NorthwestSoulmatesFeed),
    FeedFetcher.soulmates(NewNorthwestSoulmatesFeed),
    FeedFetcher.soulmates(ScotlandSoulmatesFeed),
    FeedFetcher.soulmates(YoungSoulmatesFeed),
    FeedFetcher.soulmates(MatureSoulmatesFeed),
    FeedFetcher.soulmates(WestMidlandsSoulmatesFeed),
    FeedFetcher.soulmates(EastMidlandsSoulmatesFeed),
    FeedFetcher.soulmates(YorkshireSoulmatesFeed),
    FeedFetcher.soulmates(NortheastSoulmatesFeed),
    FeedFetcher.soulmates(EastSoulmatesFeed),
    FeedFetcher.soulmates(SouthSoulmatesFeed),
    FeedFetcher.soulmates(SouthwestSoulmatesFeed),
    FeedFetcher.soulmates(WalesSoulmatesFeed)
  ).flatten

  private val refreshJobs: List[RefreshJob] = List(
    SoulmatesRefresh,
    MasterClassTagsRefresh,
    MasterclassesRefresh,
    CountriesRefresh,
    IndustriesRefresh,
    JobsRefresh,
    MoneyBestBuysRefresh,
    BooksRefresh,
    TravelOffersRefresh
  )

  override def onStart(app: PlayApp): Unit = {

    val randomFactor = Random.nextInt(15)

    def randomStartSchedule(minsLater: Int = 0) = s"0 ${randomFactor + minsLater}/15 * * * ?"

    def fetchFeed(fetcher: FeedFetcher): Unit = {

      def storeFeed(feedName: String, feed: Feed): Unit =
        S3.putPrivate(key = s"$merchandisingFeedsRoot/$feedName", value = feed.content, feed.contentType)

      def recordFetch(feedName: String, duration: Duration): Unit = {
        val key = s"${feedName.toLowerCase.replaceAll("\\s+", "-")}-feed-load-time"
        CloudWatch.put("Commercial", Map(s"$key" -> duration.toMillis.toDouble))
      }

      val feedName = fetcher.feedName
      val msgPrefix = s"Fetching $feedName feed"
      log.info(s"Fetching $feedName feed from ${fetcher.url} ...")
      val eventualResponse = fetcher.fetch()
      eventualResponse onFailure {
        case e: FetchSwitchedOff =>
          log.warn(s"$msgPrefix failed: ${e.getMessage}")
        case NonFatal(e) =>
          log.error(s"$msgPrefix failed: ${e.getMessage}")
      }
      eventualResponse onSuccess {
        case response =>
          storeFeed(feedName, response.feed)
          recordFetch(feedName, response.duration)
          log.info(s"$msgPrefix succeeded in ${response.duration}")
      }
    }

    super.onStart(app)

    for (fetcher <- feedFetchers) {
      val feedName = fetcher.feedName
      Jobs.deschedule(s"${feedName}FetchJob")
      Jobs.scheduleEveryNMinutes(s"${feedName}FetchJob", 15) {
        fetchFeed(fetcher)
      }
    }

    refreshJobs.zipWithIndex foreach {
      case (job, i) => job.start(randomStartSchedule(minsLater = i))
    }

    AkkaAsync {

      for (fetcher <- feedFetchers) {
        fetchFeed(fetcher)
      }

      SoulmatesAgent.refresh()

      MasterClassTagsAgent.refresh() andThen {
        case Success(_) => MasterClassAgent.refresh()
        case Failure(e) => log.warn(s"Failed to refresh master class tags: ${e.getMessage}")
      }

      Countries.refresh() andThen {
        case Success(_) => TravelOffersAgent.refresh()
        case Failure(e) => log.warn(s"Failed to refresh travel offer countries: ${e.getMessage}")
      }

      Industries.refresh() andThen {

        case Success(_) =>
          println("*1")
          val x = JobsAgent.refresh(FeedFetcher.jobs.map(_.feedName).getOrElse("jobs"))
          x onFailure {
            case NonFatal(e) =>
              println("*2")
              println(e)
          }
          x onSuccess {
            case r =>
              println("*3")
              println(r.jobs.size)
              println(r.parseDuration)
          }

        case Failure(e) => log.warn(s"Failed to refresh job industries: ${e.getMessage}")
      }

      BestBuysAgent.refresh()

      BestsellersAgent.refresh()
      TravelOffersRefresh.refresh()
    }
  }

  override def onStop(app: PlayApp): Unit = {
    refreshJobs foreach (_.stop())

    for (fetcher <- feedFetchers) {
      Jobs.deschedule(s"${fetcher.feedName}FetchJob")
    }

    super.onStop(app)
  }
}
