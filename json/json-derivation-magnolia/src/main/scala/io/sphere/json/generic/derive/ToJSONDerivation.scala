package io.sphere.json.generic.derive

import cats.data.ValidatedNel
import io.sphere.json.generic._
import io.sphere.json.{JSONError, ToJSON, toJValue}
import magnolia._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros

object ToJSONDerivation extends CommonDerivation {
  import CommonDerivation._

  type JSONParseResult[A] = ValidatedNel[JSONError, A]

  type Typeclass[T] = ToJSON[T]

  def deriveToJSON[T]: ToJSON[T] = macro Magnolia.gen[T]
  def toJsonProduct[T]: ToJSON[T] = macro Magnolia.gen[T]

  def combine[T <: Product](caseClass: CaseClass[ToJSON, T]): ToJSON[T] = new ToJSON[T] {
    private val jsonClass = jsonClassMetaFromCaseClass(caseClass.asInstanceOf[CaseClass[CommonDerivation.Typeclass, T]])
    private val _fields = jsonClass.fields

    override def write(r: T): JValue = {
      val buf = new ListBuffer[JField]
      if (jsonClass.typeHint.isDefined) writeTypeField(jsonClass, buf)

      var i = 0
      caseClass.parameters.foreach { p =>
        writeField(buf, _fields(i), p.dereference(r))(p.typeclass)
        i += 1
      }
      JObject(buf.toList)
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[ToJSON, T]): ToJSON[T] =
    dispatchInternal[T](sealedTrait, compactCaseObject = false)

  def dispatchInternal[T](sealedTrait: SealedTrait[ToJSON, T], compactCaseObject: Boolean): ToJSON[T] = new ToJSON[T] {

    val allSelectors = sealedTrait.subtypes.map { subType =>
      typeSelector(subType)
    }
    val readMapBuilder = Map.newBuilder[String, TypeSelector[_]]
    val writeMapBuilder = Map.newBuilder[TypeName, TypeSelector[_]]
    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
      writeMapBuilder += (s.subType.typeName -> s)
    }
    val readMap = readMapBuilder.result
    val writeMap = writeMapBuilder.result

    private val typeField = sealedTrait.annotations.collectFirst {
      case a: JSONTypeHintField => a.value
    }.getOrElse(defaultTypeFieldName)

    override def write(t: T): JValue = {
      sealedTrait.dispatch(t) { subtype =>
        writeMap.get(subtype.typeName) match {
          case None => throw new IllegalStateException("Can't find a serializer for a class " + t.getClass)
          case Some(ts) =>
            if (compactCaseObject) JString(ts.typeValue)
            else subtype.typeclass.write(subtype.cast(t)) match {
              case o @ JObject(obj) if obj.exists(_._1 == ts.typeField) => o
              case j: JObject => j ~ JField(ts.typeField, JString(ts.typeValue))
              case j => throw new IllegalStateException("The json is not an object but a " + j.getClass)
            }
        }
      }
    }
  }

  private def writeField[A: ToJSON](buf: ListBuffer[JField], field: JSONFieldMeta, e: A): Unit = {
    if (!field.ignored) {
      if (field.embedded)
        toJValue(e) match {
          case o: JObject => buf ++= o.obj
          case _ => // no update on buf
        }
      else
        buf += JField(field.name, toJValue(e))
    }
  }

  private def writeTypeField(jClass: JSONClassMeta, buf: ListBuffer[JField]): Unit =
    jClass.typeHint foreach { th =>
      buf += JField(th.field, JString(th.value))
    }
}
