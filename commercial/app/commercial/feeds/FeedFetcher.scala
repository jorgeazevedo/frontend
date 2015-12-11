package commercial.feeds

import java.lang.System.currentTimeMillis

import com.ning.http.client.Response
import conf.switches.{Switch, Switches}
import conf.{CommercialConfiguration, Configuration}
import model.commercial.soulmates.{SoulmatesAgent, SoulmatesFeed}
import org.joda.time.{DateTime, DateTimeZone}
import play.api.Play.current
import play.api.libs.ws.{WS, WSResponse}

import scala.concurrent.duration.{Duration, _}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class FeedFetcher(
  val feedName: String,
  val url: String,
  val parameters: Map[String, String],
  val timeout: Duration,
  val switch: Switch,
  responseEncoding: String = DefaultResponseEncoding()
) {

  def fetch()(implicit executionContext: ExecutionContext): Future[FetchResponse] = {

    def body(response: WSResponse): String = {
      if (responseEncoding == DefaultResponseEncoding()) {
        response.body
      } else {
        response.underlying[Response].getResponseBody(responseEncoding)
      }
    }

    def contentType(response: WSResponse): String = {
      response.underlying[Response].getContentType
    }

    switch.isGuaranteedSwitchedOn flatMap { reallyOn =>
      if (reallyOn) {

        val start = currentTimeMillis()
        val futureResponse = WS.url(url)
                             .withQueryString(parameters.toSeq: _*)
                             .withRequestTimeout(timeout.toMillis.toInt)
                             .get()

        futureResponse map { response =>
          if (response.status == 200) {
            FetchResponse(
              Feed(body(response), contentType(response)),
              Duration(currentTimeMillis() - start, MILLISECONDS)
            )
          } else {
            throw FetchException(response.status, response.statusText)
          }
        } recoverWith {
          case NonFatal(e) => Future.failed(e)
        }
      } else Future.failed(SwitchOffException(switch.name))
    }
  }
}

object FeedFetcher {

  lazy val jobs: Option[FeedFetcher] = {

    // url changes daily so cannot be val
    def url = {

      /*
       * Using offset time because this appears to be how the URL is constructed.
       * With UTC time we lose the feed for 2 hours at midnight every day.
       */
      val feedDate = new DateTime(DateTimeZone.forOffsetHours(-2)).toString("yyyy-MM-dd")

      val urlTemplate = CommercialConfiguration.getProperty("jobs.api.url.template")
      urlTemplate map (_ replace("yyyy-MM-dd", feedDate))
    }

    url map {
      new FeedFetcher("Jobs", _, Map.empty, 2.seconds, Switches.JobFeedSwitch, "UTF-8")
    }
  }

  lazy val soulmates: Seq[FeedFetcher] = {

    def soulmates(soulmatesFeed: SoulmatesFeed): Option[FeedFetcher] = {
      Configuration.commercial.soulmatesApiUrl map { url =>
        new FeedFetcher(
          soulmatesFeed.adTypeName,
          s"$url/${soulmatesFeed.path}",
          Map.empty,
          2.seconds,
          Switches.SoulmatesFeedSwitch,
          "UTF-8"
        )
      }
    }

    SoulmatesAgent.agents flatMap (agent => soulmates(agent.feed))
  }
}

object DefaultResponseEncoding {

  def apply(): String = "ISO-8859-1"
}

case class FetchResponse(feed: Feed, duration: Duration)
case class Feed(content: String, contentType: String)

final case class FetchException(status: Int, message: String) extends Exception(s"HTTP status $status: $message")

final case class SwitchOffException(switchName: String) extends Exception(s"$switchName switch is off")
