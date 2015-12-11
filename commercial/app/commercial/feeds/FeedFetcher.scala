package commercial.feeds

import java.lang.System.currentTimeMillis

import com.ning.http.client.Response
import conf.switches.{Switch, Switches}
import conf.{CommercialConfiguration, Configuration}
import model.commercial.soulmates.SoulmatesFeed
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

    switch.onInitialized flatMap { _ =>
      if (switch.isSwitchedOn) {

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
            throw FetchFailure(Some(response.status), response.statusText)
          }
        } recoverWith {
          case NonFatal(e) => Future.failed(FetchFailure(None, e.getMessage))
        }
      } else Future.failed(FetchSwitchedOff(switch.name))
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
}

object DefaultResponseEncoding {

  def apply(): String = "ISO-8859-1"
}

case class FetchResponse(feed: Feed, duration: Duration)
case class Feed(content: String, contentType: String)


sealed abstract class FetchException(message: String) extends Exception(message)

final case class FetchFailure(status: Option[Int], message: String) extends FetchException(
  message = status match {
    case Some(s) => s"HTTP status $status: $message"
    case None => message
  }
)

final case class FetchSwitchedOff(switchName: String) extends FetchException(
  message = s"$switchName switch is off"
)
