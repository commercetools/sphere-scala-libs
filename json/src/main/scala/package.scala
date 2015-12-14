package io.sphere

import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.JsonMappingException

import scalaz.{ NonEmptyList, Failure, Success, ValidationNel }
import scalaz.Validation.FlatMap._

import io.sphere.util.Logging

import org.json4s.{JsonInput, StringInput, DefaultFormats}
import org.json4s.JsonAST._
import org.json4s.ParserUtil.ParseException
import org.json4s.jackson.{ compactJson, parseJson }

/** Provides functions for reading & writing JSON, via type classes JSON/JSONR/JSONW. */
package object json extends Logging {

  implicit val liftJsonFormats = DefaultFormats

  type JValidation[A] = ValidationNel[JSONError, A]

  def parseJSON(json: JsonInput): JValidation[JValue] =
    try Success(parseJson(json)) catch {
      case e: ParseException => jsonParseError(e.getMessage)
      case e: JsonMappingException ⇒ jsonParseError(e.getOriginalMessage)
      case e: JsonParseException ⇒ jsonParseError(e.getOriginalMessage)
    }

  def parseJSON(json: String): JValidation[JValue] =
    parseJSON(StringInput(json))

  def jsonParseError[A](msg: String): Failure[NonEmptyList[JSONError]] =
    Failure(NonEmptyList(JSONParseError(msg)))

  def fromJSON[A: FromJSON](json: JsonInput): JValidation[A] =
    parseJSON(json).flatMap(fromJValue[A])

  def fromJSON[A: FromJSON](json: String): JValidation[A] =
    parseJSON(json).flatMap(fromJValue[A])

  def toJSON[A: ToJSON](a: A): String = toJValue(a) match {
    case JNothing => "{}"
    case jval => compactJson(jval)
  }

  /** Parses a JSON string into a type A.
   *  Throws a [[JSONException]] on failure.
   *
   * @param json The JSON string to parse.
   * @return An instance of type A. */
  def getFromJSON[A: FromJSON](json: JsonInput): A =
    getFromJValue[A](parseJson(json))

  def getFromJSON[A: FromJSON](json: String): A =
    getFromJSON(StringInput(json))

  def fromJValue[A](jval: JValue)(implicit json: FromJSON[A]): JValidation[A] =
    json.read(jval)

  def toJValue[A](a: A)(implicit json: ToJSON[A]): JValue =
    json.write(a)

  def getFromJValue[A: FromJSON](jval: JValue): A =
    fromJValue[A](jval) match {
      case Success(a) => a
      case Failure(errs) => throw new JSONException(errs.list.toList.mkString(", "))
    }

  /** Extracts a JSON value of type A from a named field of a JSON object.
    *
    * @param name The name of the field.
    * @param jval The JValue from which to extract the field.
    * @return A success with a value of type A or a non-empty list of errors. */
  def field[A](
    name: String,
    default: Option[A] = None
  )(jval: JValue)(implicit jsonr: FromJSON[A]): JValidation[A] = jval match {
    case JObject(fields) =>
      fields.find(_._1 == name).filterNot(_._2 == JNull)
        .map(f => jsonr.read(f._2).fold(
          errs => Failure(errs map[JSONError] {
            case JSONParseError(msg) => JSONFieldError(List(name), msg)
            case JSONFieldError(path, msg) => JSONFieldError(name :: path, msg)
          }), Success(_)))
        .orElse(default.map(Success(_)))
        .orElse(jsonr.read(JNothing).fold(_ => None, x => Some(Success(x)))) // orElse(jsonr.default)
        .getOrElse(Failure(NonEmptyList(JSONFieldError(List(name), "Missing required value"))))
    case x => jsonParseError("Expected JSON object. Got '" + compactJson(x) + "'")
  }
}
