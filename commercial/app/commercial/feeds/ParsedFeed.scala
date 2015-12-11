package commercial.feeds

import scala.concurrent.duration.Duration

case class ParsedFeed[T](contents: Seq[T], parseDuration: Duration)

sealed trait ParseException extends Exception
case object MissingFeedException extends ParseException
