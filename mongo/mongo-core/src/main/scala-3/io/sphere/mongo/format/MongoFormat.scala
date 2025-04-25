package io.sphere.mongo.format

import com.mongodb.BasicDBObject
import io.sphere.mongo.generic.{AnnotationReader, Field}
import org.bson.types.ObjectId

import java.util.UUID
import java.util.regex.Pattern
import scala.deriving.Mirror

type SimpleMongoType = UUID | String | ObjectId | Short | Int | Long | Float | Double | Boolean |
  Pattern

trait MongoFormat[A] extends Serializable {
  def toMongoValue(a: A): Any
  def fromMongoValue(mongoType: Any): A

  //  /** needed JSON fields - ignored if empty */
  val fields: Set[String] = MongoFormat.emptyFields

  def default: Option[A] = None
}

inline def deriveMongoFormat[A](using Mirror.Of[A]): MongoFormat[A] = MongoFormat.derived

object MongoFormat {
  inline def apply[A: MongoFormat]: MongoFormat[A] = summon

  private val emptyFields: Set[String] = Set.empty

  inline given derived[A](using Mirror.Of[A]): MongoFormat[A] = Derivation.derived

  private def addField(bson: BasicDBObject, field: Field, mongoType: Any): Unit =
    if (!field.ignored)
      mongoType match {
        case s: SimpleMongoType => bson.put(field.name, s)
        case innerBson: BasicDBObject =>
          if (field.embedded) innerBson.entrySet().forEach(p => bson.put(p.getKey, p.getValue))
          else bson.put(field.name, innerBson)
        case MongoNothing =>
      }

  private object Derivation {

    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): MongoFormat[A] =
      inline m match {
        case s: Mirror.SumOf[A] => deriveTrait(s)
        case p: Mirror.ProductOf[A] => deriveCaseClass(p)
      }

    @annotation.nowarn("msg=New anonymous class definition will be duplicated at each inline site")
    inline private def deriveTrait[A](mirrorOfSum: Mirror.SumOf[A]): MongoFormat[A] =
      new MongoFormat[A] {
        private val traitMetaData = AnnotationReader.readTraitMetaData[A]
        private val typeHintMap = traitMetaData.subTypeTypeHints
        private val reverseTypeHintMap = typeHintMap.map((on, n) => (n, on))
        private val formatters = summonFormatters[mirrorOfSum.MirroredElemTypes]
        private val names = constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector
          .asInstanceOf[Vector[String]]
        private val formattersByTypeName = names.zip(formatters).toMap

        override def toMongoValue(a: A): Any = {
          // we never get a trait here, only classes, it's safe to assume Product
          val originalTypeName = a.asInstanceOf[Product].productPrefix
          val typeName = typeHintMap.getOrElse(originalTypeName, originalTypeName)
          val bson =
            formattersByTypeName(originalTypeName).toMongoValue(a).asInstanceOf[BasicDBObject]
          bson.put(traitMetaData.typeDiscriminator, typeName)
          bson
        }

        override def fromMongoValue(bson: Any): A =
          bson match {
            case bson: BasicDBObject =>
              val typeName = bson.get(traitMetaData.typeDiscriminator).asInstanceOf[String]
              val originalTypeName = reverseTypeHintMap.getOrElse(typeName, typeName)
              formattersByTypeName(originalTypeName).fromMongoValue(bson).asInstanceOf[A]
            case x =>
              throw new Exception(s"BsonObject is expected for a Trait subtype, instead got $x")
          }
      }

    @annotation.nowarn("msg=New anonymous class definition will be duplicated at each inline site")
    inline private def deriveCaseClass[A](mirrorOfProduct: Mirror.ProductOf[A]): MongoFormat[A] =
      new MongoFormat[A] {
        private val caseClassMetaData = AnnotationReader.readCaseClassMetaData[A]
        private val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
        private val fieldsAndFormatters = caseClassMetaData.fields.zip(formatters)

        override val fields: Set[String] = fieldsAndFormatters.toSet.flatMap((field, formatter) =>
          if (field.embedded) formatter.fields + field.rawName
          else Set(field.rawName))

        override def toMongoValue(a: A): Any = {
          val bson = new BasicDBObject()
          val values = a.asInstanceOf[Product].productIterator
          formatters.zip(values).zip(caseClassMetaData.fields).foreach {
            case ((format, value), field) =>
              addField(bson, field, format.toMongoValue(value))
          }
          bson
        }

        override def fromMongoValue(mongoType: Any): A =
          mongoType match {
            case bson: BasicDBObject =>
              val fields = fieldsAndFormatters
                .map { case (field, format) =>
                  def defaultValue = field.defaultArgument.orElse(format.default)

                  if (field.ignored)
                    defaultValue.getOrElse {
                      throw new Exception(
                        s"Missing default parameter value for ignored field `${field.name}` on deserialization.")
                    }
                  else if (field.embedded) format.fromMongoValue(bson)
                  else {
                    val value = bson.get(field.name)
                    if (value ne null) format.fromMongoValue(value.asInstanceOf[Any])
                    else
                      defaultValue.getOrElse {
                        throw new Exception(
                          s"Missing required field '${field.name}' on deserialization.")
                      }
                  }
                }
              val tuple = Tuple.fromArray(fields.toArray)
              mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

            case x => throw new Exception(s"BasicDBObject is expected for a class, instead got: $x")
          }
      }

    inline private def summonFormatters[T <: Tuple]: Vector[MongoFormat[Any]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) =>
          summonInline[MongoFormat[t]]
            .asInstanceOf[MongoFormat[Any]] +: summonFormatters[ts]
      }

  }
}
