package io.sphere

import scalaz.{ NonEmptyList, Failure, Success, ValidationNel }

import io.sphere.util.Logging

import net.liftweb.json.{ DefaultFormats, JsonParser }
import net.liftweb.json.JsonAST._

/** Provides functions for reading & writing JSON, via type classes JSON/JSONR/JSONW. */
package object json extends Logging {

  implicit val liftJsonFormats = DefaultFormats

  type JValidation[A] = ValidationNel[JSONError, A]

  def parseJSON(json: String): JValidation[JValue] =
    try Success(JsonParser.parse(json)) catch {
      case e: JsonParser.ParseException => jsonParseError(e.getMessage)
    }

  def jsonParseError[A](msg: String): Failure[NonEmptyList[JSONError]] =
    Failure(NonEmptyList(JSONParseError(msg)))

  def fromJSON[A: FromJSON](json: String): JValidation[A] =
    parseJSON(json).flatMap(fromJValue[A])

  def toJSON[A: ToJSON](a: A): String = toJValue(a) match {
    case JNothing => "{}"
    case jval => compactRender(jval)
  }

  /** Parses a JSON string into a type A.
   *  Throws a [[JSONException]] on failure.
   *
   * @param json The JSON string to parse.
   * @return An instance of type A. */
  def getFromJSON[A: FromJSON](json: String): A =
    getFromJValue[A](JsonParser.parse(json))

  def fromJValue[A: FromJSON](jval: JValue): JValidation[A] =
    implicitly[FromJSON[A]].read(jval)

  def toJValue[A: ToJSON](a: A): JValue =
    implicitly[ToJSON[A]].write(a)

  def getFromJValue[A: FromJSON](jval: JValue): A =
    fromJValue[A](jval) match {
      case Success(a) => a
      case Failure(errs) => throw new JSONException(errs.list.mkString(", "))
    }

  /** Extracts a JSON value of type A from a named field of a JSON object.
    *
    * @param name The name of the field.
    * @param jval The JValue from which to extract the field.
    * @return A success with a value of type A or a non-empty list of errors. */
  def field[A: FromJSON](
    name: String,
    default: Option[A] = None
  )(jval: JValue): JValidation[A] = jval match {
    case JObject(fields) =>
      val jsonr = implicitly[FromJSON[A]]
      fields.find(_.name == name)
        .map(f => jsonr.read(f.value).fold(
          errs => Failure(errs map {
            case JSONParseError(msg) => JSONFieldError(List(name), msg)
            case JSONFieldError(path, msg) => JSONFieldError(name :: path, msg)
          }), Success(_)))
        .orElse(default.map(Success(_)))
        .orElse(jsonr.read(JNothing).fold(_ => None, x => Some(Success(x)))) // orElse(jsonr.default)
        .getOrElse(Failure(NonEmptyList(JSONFieldError(List(name), "Missing required value"))))
    case x => jsonParseError("Expected JSON object. Got '" + compactRender(x) + "'")
  }
}
