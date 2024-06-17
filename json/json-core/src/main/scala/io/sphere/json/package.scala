package io.sphere

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.core.exc.{InputCoercionException, StreamConstraintsException}
import com.fasterxml.jackson.databind.JsonMappingException
import io.sphere.util.Logging
import org.json4s.{DefaultFormats, JsonInput, StringInput}
import org.json4s.JsonAST._
import org.json4s.ParserUtil.ParseException
import org.json4s.jackson.compactJson

/** Provides functions for reading & writing JSON, via type classes JSON/JSONR/JSONW. */
package object json extends Logging {

  implicit val liftJsonFormats: DefaultFormats = DefaultFormats

  type JValidation[A] = ValidatedNel[JSONError, A]

  def parseJsonUnsafe(json: JsonInput): JValue =
    SphereJsonParser.parse(json, useBigDecimalForDouble = false, useBigIntForLong = false)

  def parseJSON(json: JsonInput): JValidation[JValue] =
    try Valid(parseJsonUnsafe(json))
    catch {
      case e: ParseException => jsonParseError(e.getMessage)
      case e: JsonMappingException => jsonParseError(e.getOriginalMessage)
      case e: JsonParseException => jsonParseError(e.getOriginalMessage)
      case e: InputCoercionException => jsonParseError(e.getOriginalMessage)
      case e: StreamConstraintsException => jsonParseError(e.getOriginalMessage)
    }

  def parseJSON(json: String): JValidation[JValue] =
    parseJSON(StringInput(json))

  def jsonParseError[A](msg: String): Invalid[NonEmptyList[JSONError]] =
    Invalid(NonEmptyList.one(JSONParseError(msg)))

  def fromJSON[A: FromJSON](json: JsonInput): JValidation[A] =
    parseJSON(json).andThen(fromJValue[A])

  def fromJSON[A: FromJSON](json: String): JValidation[A] =
    parseJSON(json).andThen(fromJValue[A])

  private val jNothingStr = "{}"

  def toJSON[A: ToJSON](a: A): String = toJValue(a) match {
    case JNothing => jNothingStr
    case jval => compactJson(jval)
  }

  /** Parses a JSON string into a type A. Throws a [[JSONException]] on failure.
    *
    * @param json
    *   The JSON string to parse.
    * @return
    *   An instance of type A.
    */
  def getFromJSON[A: FromJSON](json: JsonInput): A =
    getFromJValue[A](parseJsonUnsafe(json))

  def getFromJSON[A: FromJSON](json: String): A =
    getFromJSON(StringInput(json))

  def fromJValue[A](jval: JValue)(implicit json: FromJSON[A]): JValidation[A] =
    json.read(jval)

  def toJValue[A](a: A)(implicit json: ToJSON[A]): JValue =
    json.write(a)

  def getFromJValue[A: FromJSON](jval: JValue): A =
    fromJValue[A](jval) match {
      case Valid(a) => a
      case Invalid(errs) => throw new JSONException(errs.toList.mkString(", "))
    }

  /** Extracts a JSON value of type A from a named field of a JSON object.
    *
    * @param name
    *   The name of the field.
    * @param jObject
    *   The JObject from which to extract the field.
    * @return
    *   A success with a value of type A or a non-empty list of errors.
    */
  def field[A](
      name: String,
      default: Option[A] = None
  )(jObject: JObject)(implicit jsonr: FromJSON[A]): JValidation[A] = {
    val fields = jObject.obj
    // Perf note: avoiding Some(f) with fields.indexWhere and then constant time access is not faster
    fields.find(f => f._1 == name && f._2 != JNull && f._2 != JNothing) match {
      case Some(f) =>
        jsonr
          .read(f._2)
          .leftMap(errs =>
            errs.map {
              case JSONParseError(msg) => JSONFieldError(name :: Nil, msg)
              case JSONFieldError(path, msg) => JSONFieldError(name :: path, msg)
            })
      case None =>
        default
          .map(Valid(_))
          .orElse(
            jsonr.read(JNothing).fold(_ => None, x => Some(Valid(x)))
          ) // orElse(jsonr.default)
          .getOrElse(
            Invalid(NonEmptyList.one(JSONFieldError(name :: Nil, "Missing required value"))))
    }
  }
}
