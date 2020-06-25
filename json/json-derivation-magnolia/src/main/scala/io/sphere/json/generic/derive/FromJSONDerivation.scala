package io.sphere.json.generic.derive

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import io.sphere.json.generic._
import io.sphere.json.{FromJSON, JSONError, JSONException, JValidation, field, fromJValue, jsonParseError}
import magnolia._
import org.json4s.JsonAST._
import org.json4s.jackson.compactJson

import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros

object FromJSONDerivation extends CommonDerivation {
  import CommonDerivation._

  type JSONParseResult[A] = ValidatedNel[JSONError, A]

  type Typeclass[T] = FromJSON[T]

  def deriveJSON[T]: FromJSON[T] = macro Magnolia.gen[T]
  def fromJsonProduct[T]: FromJSON[T] = macro Magnolia.gen[T]

  def combine[T <: Product](caseClass: CaseClass[FromJSON, T]): FromJSON[T] = new FromJSON[T] {
    private val jsonClass = jsonClassMetaFromCaseClass(caseClass.asInstanceOf[CaseClass[CommonDerivation.Typeclass, T]])
    private val _fields = jsonClass.fields

    override def read(jval: JValue): JValidation[T] = jval match {
      case o: JObject =>
        var i = -1
        // "imperative" style for performances
        val initial: JValidation[ListBuffer[Any]] = Valid(new ListBuffer())
        val fieldValues = caseClass.parameters.foldLeft(initial) { (acc, p) =>
          i += 1
          val result = readField(_fields(i), o)(p.typeclass)
          (acc, result) match {
            case (Valid(builder), Valid(e)) => Valid(builder += e)
            case (Valid(_), i @ Invalid(_)) => i
            case (Invalid(e1), Invalid(e2)) => Invalid(e1.concatNel(e2))
            case (i @ Invalid(_), _) => i
          }
        }.map(_.result())

        fieldValues.map(caseClass.rawConstruct)
      case _ => jsonParseError("JSON object expected.")
    }

    override val fields: Set[String] = calculateFields()
    private def calculateFields(): Set[String] = {
      val builder = Set.newBuilder[String]
      var i = 0
      caseClass.parameters.foreach { p =>
        val f = _fields(i)
        if (!f.ignored) {
          if (f.embedded)
            builder ++= p.typeclass.fields
          else
            builder += f.name
        }
        i += 1
      }
      builder.result()
    }

  }

  def dispatch[T](sealedTrait: SealedTrait[FromJSON, T]): FromJSON[T] =
    dispatchInternal[T](sealedTrait, compactCaseObject = false)

  def dispatchInternal[T](sealedTrait: SealedTrait[FromJSON, T], compactCaseObject: Boolean): FromJSON[T] = new FromJSON[T] {

    private val allSelectors = sealedTrait.subtypes.map { subType =>
      typeSelector(subType)
    }
    private val readMapBuilder = Map.newBuilder[String, TypeSelector[_]]
    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
    }
    private val readMap = readMapBuilder.result

    private val typeField = sealedTrait.annotations.collectFirst {
      case a: JSONTypeHintField => a.value
    }.getOrElse(defaultTypeFieldName)

    override def read(jval: JValue): JValidation[T] = {
      if (compactCaseObject) {
        jval match {
          case s @ JString(typeName) =>
            readMap.get(typeName) match {
              case Some(ts) => ts.subType.typeclass.read(s).asInstanceOf[JValidation[T]]
              case None => jsonParseError("Invalid value '" + typeName + "'.")
            }

          case _ => jsonParseError("JSON string expected.")
        }
      } else {
        jval match {
          case o: JObject => findTypeValue(o, typeField) match {
            case Some(t) => readMap.get(t) match {
              case Some(ts) => ts.subType.typeclass.read(jval).asInstanceOf[JValidation[T]]
              case None => jsonParseError("Invalid type value '" + t + "' in '%s'".format(compactJson(o)))
            }
            case None => jsonParseError("Missing type field '" + typeField + "' in '%s'".format(compactJson(o)))
          }
          case _ => jsonParseError("JSON object expected.")
        }
      }
    }
  }

  private def readField[A: FromJSON](f: JSONFieldMeta, o: JObject): JSONParseResult[A] = {
    val default = f.default.asInstanceOf[Option[A]]
    if (f.ignored) default.map(Valid(_))/*.orElse(jsonr.default)*/.getOrElse {
      // programmer error
      throw new JSONException("Missing default for ignored field.")
    }
    else if (f.embedded) fromJValue[A](o)
    else field[A](f.name, default)(o)
  }

  private def findTypeValue(o: JObject, typeField: String): Option[String] = {
    import io.sphere.json.liftJsonFormats
    o.obj.find(_._1 == typeField).flatMap(_._2.extractOpt[String])
  }
}
