package implicits

import com.amazonaws.services.dynamodbv2.model.AttributeValue
import play.api.libs.json._
import scala.collection.JavaConverters._

object JsonImplicits {
  implicit class FoldableJsValue(jsValue: JsValue) {
    def fold[B](jsNull: => B,
                jsBool: Boolean => B,
                jsNumber: BigDecimal => B,
                jsString: String => B,
                jsArray: Seq[JsValue] => B,
                jsObject: Seq[(String, JsValue)] => B,
                jsUndefined: => B): B = jsValue match {
      case JsNull => jsNull
      case JsBoolean(b) => jsBool(b)
      case JsNumber(n) => jsNumber(n)
      case JsString(s) => jsString(s)
      case JsArray(a) => jsArray(a)
      case JsObject(o) => jsObject(o.toSeq)
      case JsUndefined() => jsUndefined
    }
  }
}
