package io.sphere.json.generic.derive

import io.sphere.json.generic._
import io.sphere.util.{Logging, Memoizer}
import magnolia.{CaseClass, Subtype, TypeName}

trait CommonDerivation extends Logging {
  import CommonDerivation._

  type Typeclass[T]

  protected case class TypeSelector[A](val typeField: String, val typeValue: String, subType: Subtype[Typeclass, A])

  protected def typeSelector[A](subType: Subtype[Typeclass, A]): TypeSelector[A] = {
    val (typeField, typeValue) = jsonClassMetaFromSubtype(subType.asInstanceOf[Subtype[CommonDerivation.Typeclass, A]]).typeHint match {
      case Some(hint) => (hint.field, hint.value)
      case None => (defaultTypeFieldName, defaultTypeValue(subType.typeName))
    }
    new TypeSelector[A](typeField, typeValue, subType)
  }
}

object CommonDerivation extends Logging {

  // used as common type class thanks to type erasure
  type Typeclass[_]

  private[derive] val defaultTypeFieldName: String = JSONTypeHintField.defaultValue

  private[derive] case class JSONClassMeta(typeHint: Option[JSONClassMeta.TypeHint], fields: IndexedSeq[JSONFieldMeta])
  private[derive] object JSONClassMeta {
    case class TypeHint(field: String, value: String)
  }
  private[derive] case class JSONFieldMeta(
    name: String,
    default: Option[Any] = None,
    embedded: Boolean = false,
    ignored: Boolean = false)

  private[derive] val jsonClassMetaFromCaseClass = new Memoizer[CaseClass[Typeclass, _], JSONClassMeta](caseClass => {
    log.trace("Initializing JSON metadata for %s".format(caseClass.typeName.full))

    JSONClassMeta(
      typeHint = typeHint(caseClass.typeName, caseClass.annotations),
      fields = getJSONFieldMeta(caseClass)
    )
  })

  private def getJSONFieldMeta(caseClass: CaseClass[Typeclass, _]): IndexedSeq[JSONFieldMeta] = {
    caseClass.parameters.map { p =>
      val annotations = p.annotations
      val name = annotations.collectFirst {
        case h: JSONKey => h
      }.fold(p.label)(_.value)
      val embedded = annotations.exists {
        case _: JSONEmbedded => true
        case _ => false
      }
      val ignored = annotations.exists {
        case _: JSONIgnore => true
        case _ => false
      }
      if (ignored && p.default.isEmpty) {
        throw new Exception("Ignored Mongo field '%s' must have a default value.".format(p.label))
      }
      JSONFieldMeta(name, p.default, embedded, ignored)
    }.toIndexedSeq
  }

  private val jsonClassMetaFromSubtype = new Memoizer[Subtype[Typeclass, _], JSONClassMeta](subType => {
    log.trace("Initializing Mongo metadata for %s".format(subType.typeName.full))

    JSONClassMeta(
      typeHint = typeHint(subType.typeName, subType.annotations),
      fields = IndexedSeq[JSONFieldMeta]()
    )
  })

  private def typeHint(typeName: TypeName, annotations: Seq[Any]): Option[JSONClassMeta.TypeHint] = {
    def hintVal(h: JSONTypeHint): String =
      if (h.value.isEmpty) defaultTypeValue(typeName)
      else h.value

    val typeHintFieldAnnot: Option[JSONTypeHintField] = annotations.collectFirst {
      case h: JSONTypeHintField => h
    }
    val typeHintAnnot: Option[JSONTypeHint] = annotations.collectFirst {
      case h: JSONTypeHint => h
    }
    val typeField = typeHintFieldAnnot.map(_.value)
    val typeValue = typeHintAnnot.map(hintVal)

    (typeField, typeValue) match {
        case (Some(field), Some(hint)) => Some(JSONClassMeta.TypeHint(field, hint))
        case (None       , Some(hint)) => Some(JSONClassMeta.TypeHint(defaultTypeFieldName, hint))
        case (Some(field), None)       => Some(JSONClassMeta.TypeHint(field, defaultTypeValue(typeName)))
        case (None       , None)       => None
      }
  }

  private[derive] def defaultTypeValue(typeName: TypeName): String =
    typeName.short.replace("$", "")
}
