package commercial.feeds

import model.commercial.jobs.Job

import scala.concurrent.duration.Duration

case class ParsedFeed(jobs: Seq[Job], parseDuration: Duration)

sealed trait ParseException extends Exception
case object MissingFeedException extends ParseException
