package io.sphere.mongo.generic

import com.mongodb.BasicDBObject
import io.sphere.mongo.format.{MongoFormat, TraitMongoFormat}
import org.bson.BSONObject

import scala.compiletime.summonInline
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.reflect.ClassTag

inline def deriveMongoFormat[A](using Mirror.Of[A]): MongoFormat[A] = MongoFormat.derived

def mongoEnum(e: Enumeration): MongoFormat[e.Value] = new MongoFormat[e.Value] {
  def toMongoValue(a: e.Value): Any = a.toString

  def fromMongoValue(any: Any): e.Value = e.withName(any.asInstanceOf[String])
}

/** One subtype of a type switch, its formatters already resolved. If `A` is itself a trait these
  * are that trait's whole switch, so `typeSelectors` holds one entry per `sub`, not one per leaf
  * subtype as on Scala 2.
  */
final case class TypeSelector[A](
    readFormatters: Map[String, MongoFormat[Any]],
    writeFormatters: Map[Class[?], (String, MongoFormat[Any])])

trait MongoTypeSelectorContainer {
  def typeSelectors: List[TypeSelector[?]]
}

/** Builds the selector for the subtype `A` of a `mongoTypeSwitch`. Inline only to summon and to
  * read the name; the work is in the plain `selector` def, which keeps a call site small enough
  * that hundreds of subtypes fit in one method (the JVM caps it at 64KB).
  */
inline def sub[A]: TypeSelector[A] =
  selector(
    MongoAnnotationReader.readSerializedName[A],
    summonInline[ClassTag[A]],
    summonInline[MongoFormat[A]])

private def selector[A](
    serializedName: String,
    classTag: ClassTag[A],
    format: MongoFormat[A]): TypeSelector[A] =
  format match {
    // A trait subtype brings its own subtype table.
    case traitFormat: TraitMongoFormat[A] => traitFormat.typeSelector
    case _ =>
      val anyFormat = format.asInstanceOf[MongoFormat[Any]]
      TypeSelector(
        readFormatters = Map(serializedName -> anyFormat),
        writeFormatters = Map(classTag.runtimeClass -> (serializedName, anyFormat)))
  }

/** Creates a `MongoFormat[T]` that switches on a type-hint field, delegating to the given subtype
  * selectors' instances.
  */
inline def mongoTypeSwitch[T](
    selectors: List[TypeSelector[?]]): MongoFormat[T] with MongoTypeSelectorContainer = {
  require(selectors.nonEmpty, "mongoTypeSwitch needs at least one subtype")
  typeSwitchInstance[T](selectors, MongoAnnotationReader.readTypeDiscriminator[T])
}

private def typeSwitchInstance[T](
    selectors: List[TypeSelector[?]],
    discriminator: String): MongoFormat[T] with MongoTypeSelectorContainer = {
  // The discriminator always comes from `T`, so a merged child's entries get stamped with the
  // parent's field rather than their own.
  val merged = TypeSelector[T](
    readFormatters = selectors.map(_.readFormatters).fold(Map.empty)(_ ++ _),
    writeFormatters = selectors.map(_.writeFormatters).fold(Map.empty)(_ ++ _))

  new TraitMongoFormat[T] with MongoTypeSelectorContainer {
    override val typeSelector: TypeSelector[T] = merged
    override def typeSelectors: List[TypeSelector[?]] = selectors

    override def toMongoValue(a: T): Any = {
      val (serializedName, format) = merged.writeFormatters(a.getClass)
      format.toMongoValue(a) match {
        case bson: BasicDBObject =>
          // Leave the discriminator untouched if the subtype's own formatter already wrote it.
          if (bson.containsField(discriminator)) bson
          else {
            bson.put(discriminator, serializedName)
            bson
          }
        case any =>
          throw new Exception(s"BasicDBObject expected but got ${any.getClass.getName}.")
      }
    }

    override def fromMongoValue(any: Any): T = any match {
      case bson: BasicDBObject =>
        val serializedTypeName = findTypeValue(bson, discriminator).getOrElse(
          throw new Exception(s"Missing type field '$discriminator' in DBObject '$bson'."))
        merged.readFormatters
          .getOrElse(
            serializedTypeName,
            throw new Exception(s"Invalid type value '$serializedTypeName' in DBObject '$bson'."))
          .fromMongoValue(bson)
          .asInstanceOf[T]
      case x =>
        throw new Exception(s"DBObject expected but got ${x.getClass.getName}.")
    }
  }
}

private def findTypeValue(dbo: BSONObject, typeField: String): Option[String] =
  Option(dbo.get(typeField)).map(_.toString)

/** Bridges a `Mirror`'s `MirroredElemTypes` to the selector list `mongoTypeSwitch` takes; only
  * `derived` needs it. A macro rather than an inline recursion, which would nest one expansion per
  * subtype and exhaust `-Xmax-inlines` at ~24.
  */
private[mongo] inline def subsOf[T <: Tuple]: List[TypeSelector[?]] = ${ subsOfImpl[T] }

private def subsOfImpl[T <: Tuple: Type](using Quotes): Expr[List[TypeSelector[?]]] =
  Expr.ofList(tupleElemTypes[T].map { case '[t] => '{ sub[t] } })

private def tupleElemTypes[T: Type](using Quotes): List[Type[?]] =
  Type.of[T] match {
    case '[EmptyTuple] => Nil
    case '[head *: tail] => Type.of[head] :: tupleElemTypes[tail]
    case _ =>
      quotes.reflect.report.errorAndAbort("Expected a tuple of subtypes, got " + Type.show[T])
  }
