package model

import com.gu.contentapi.client.model.v1.{AssetFields, Asset}
import views.support.{Orientation, Naked, ImgSrc}

case class ImageAsset(delegate: Asset, index: Int) {
  private lazy val fields: Option[AssetFields] = delegate.typeData

  lazy val mediaType: String = delegate.`type`.name
  lazy val mimeType: Option[String] = delegate.mimeType

  lazy val url: Option[String] = delegate.file
  lazy val path: Option[String] = url.map(ImgSrc(_, Naked))

  lazy val thumbnail: Option[String] = fields.flatMap(_.thumbnail)
  lazy val thumbnailPath: Option[String] = thumbnail.map(ImgSrc(_, Naked))

  lazy val width: Int = fields.flatMap(_.width).getOrElse(1)
  lazy val height: Int = fields.flatMap(_.height).getOrElse(1)
  lazy val ratio: Int = width/height
  lazy val role: Option[String] = fields.map(_.role)
  lazy val orientation: Orientation = Orientation.fromDimensions(width, height)

  lazy val caption: Option[String] = fields.flatMap(_.caption)
  lazy val altText: Option[String] = fields.flatMap(_.altText)
  lazy val mediaId: Option[String] = fields.mediaId

  lazy val source: Option[String] = fields.flatMap(_.source)
  lazy val photographer: Option[String] = fields.flatMap(_.photographer)
  lazy val credit: Option[String] = fields.flatMap(_.credit)
  lazy val displayCredit: Boolean = fields.flatMap(_.displayCredit).getOrElse(false)
  lazy val isMaster: Boolean = fields.flatMap(_.isMaster).getOrElse(false)

  def showCaption = caption.exists(_.trim.nonEmpty) || (displayCredit && credit.nonEmpty)

  lazy val creditEndsWithCaption = (for {
    credit <- credit
    caption <- caption
  } yield caption.endsWith(credit)).getOrElse(false)
}

case class VideoAsset(private val delegate: Asset, image: Option[ImageContainer]) {

  private lazy val fields: Option[AssetFields] = delegate.typeData

  lazy val url: Option[String] = delegate.file
  lazy val mimeType: Option[String] = delegate.mimeType
  lazy val width: Int = fields.flatMap(_.width).getOrElse(0)
  lazy val height: Int = fields.flatMap(_.height).getOrElse(0)
  lazy val encoding: Option[Encoding] = {
    (url, mimeType) match {
      case (Some(url), Some(mimeType)) => Some(Encoding(url, mimeType))
      case _ => None
    }
  }

  // The video duration in seconds
  lazy val duration: Int = fields.flatMap(_.durationSeconds).getOrElse(0) +
                           fields.flatMap(_.durationMinutes).map(_ * 60).getOrElse(0)
  lazy val blockVideoAds: Boolean = fields.blockAds.exists(_.toBoolean)

  lazy val source: Option[String] = fields.flatMap(_.source)
  lazy val embeddable: Option[Boolean] = fields.flatMap(_.embeddable)
  lazy val caption: Option[String] = fields.flatMap(_.caption)
}

case class AudioAsset(private val delegate: Asset) {

  private lazy val fields: Option[AssetFields] = delegate.typeData

  lazy val url: Option[String] = delegate.file
  lazy val mimeType: Option[String] = delegate.mimeType

  // The audio duration in seconds
  lazy val duration: Int = fields.flatMap(_.durationSeconds).getOrElse(0) +
    fields.flatMap(_.durationMinutes).map(_ * 60).getOrElse(0)
}

case class EmbedAsset(private val delegate: Asset) { // richard interactiveElementFields

  private lazy val fields: Option[AssetFields] = delegate.typeData

  lazy val url: Option[String] = delegate.file
  lazy val iframeUrl: Option[String] = fields.flatMap(_.iframeUrl)
  lazy val scriptName: Option[String] = fields.flatMap(_.scriptName)
  lazy val source: Option[String] = fields.flatMap(_.source)
  lazy val scriptUrl: Option[String] = fields.flatMap(_.scriptUrl)
  lazy val caption: Option[String] = fields.flatMap(_.caption)
  lazy val html: Option[String] = fields.flatMap(_.html)
  lazy val embedType: Option[String] = fields.flatMap(_.embedType)
  lazy val role: Option[String] = fields.flatMap(_.role)
}
