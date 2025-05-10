package io.sphere.json.generic

import org.json4s.JsonAST.*
import cats.syntax.validated.*
import io.sphere.json.*

import scala.collection.mutable

object EnumerationInstances {
  def toJsonEnum(e: Enumeration): ToJSON[e.Value] = new ToJSON[e.Value] {
    def write(a: e.Value): JValue = JString(a.toString)
  }

  /** Creates a FromJSON instance for an Enumeration type that encodes the `toString`
    * representations of the enumeration values.
    */
  def fromJsonEnum(e: Enumeration): FromJSON[e.Value] = {
    // We're using an AnyRefMap for performance, it's not for mutability.
    val validRepresentations = mutable.AnyRefMap(
      e.values.iterator.map(value => value.toString -> value.validNel[JSONError]).toSeq: _*
    )

    val allowedValues = e.values.mkString("'", "','", "'")

    new FromJSON[e.Value] {
      override def read(json: JValue): JValidation[e.Value] =
        json match {
          case JString(string) =>
            validRepresentations.getOrElse(
              string,
              jsonParseError(
                "Invalid enum value: '%s'. Expected one of: %s".format(string, allowedValues)))
          case _ => jsonParseError("JSON String expected.")
        }
    }
  }

  // This can be used instead of deriveJSON
  def jsonEnum(e: Enumeration): JSON[e.Value] = {
    val toJson = toJsonEnum(e)
    val fromJson = fromJsonEnum(e)

    new JSON[e.Value] {
      override def read(jval: JValue): JValidation[e.Value] = fromJson.read(jval)

      override def write(value: e.Value): JValue = toJson.write(value)
    }
  }
}
