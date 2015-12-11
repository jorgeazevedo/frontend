package commercial.feeds

import conf.Configuration.commercial.merchandisingFeedsRoot
import model.commercial.jobs.Job
import play.api.libs.json.Json
import services.S3

import scala.concurrent.Future

// todo switch off
// todo unparseable
// todo no file
class FeedParser(key:String) {

  def parse(): Future[Seq[Job]] = {
    val x = S3.get(key)
    val y = x map (Json.parse(_))
//    val z = Job(xml)
    Future.successful(Nil)
  }
}

object FeedParser {

  def jobs():FeedParser = {
    new FeedParser(s"$merchandisingFeedsRoot/jobs")
  }
}
