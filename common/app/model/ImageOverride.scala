package model

import com.gu.contentapi.client.model.v1.{Element => ApiElement, AssetFields, ElementType, Asset, AssetType}
import org.joda.time.DateTime

object ImageOverride {

  def createElementWithOneAsset(imageSrc: String, width: Option[String], height: Option[String]): ApiElement = {
    val widthAndHeightAssetFields = Some(AssetFields(width = width.map(_.toInt), height = height.map(_.toInt)))

    ApiElement(
      id = "override",
      relation = "thumbnail",
      `type` = ElementType.Image,
      galleryIndex = None,
      assets = List(Asset(
        `type` = AssetType.Image,
        mimeType = Option("image/jpg"),
        file = Option(imageSrc),
        typeData = widthAndHeightAssetFields
      ))
    )
  }

  def createElementWithOneAsset(imageSrc: String, width: String, height: String): ApiElement =
    createElementWithOneAsset(imageSrc, Option(width), Option(height))

}
