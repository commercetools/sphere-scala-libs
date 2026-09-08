package io.sphere.json.generic

import io.sphere.json.*
import io.sphere.json.generic.JSONTypeSwitch.{FromFormatters, ToFormatters}
import org.json4s.JsonAST.JValue

import scala.compiletime.summonInline
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type, quotes}
import scala.reflect.ClassTag

inline def deriveJSON[A](using Mirror.Of[A]): JSON[A] = JSON.derived
inline def deriveToJSON[A](using Mirror.Of[A]): ToJSON[A] = ToJSON.derived
inline def deriveFromJSON[A](using Mirror.Of[A]): FromJSON[A] = FromJSON.derived

/** Creates a ToJSON instance for an Enumeration type that encodes the `toString` representations of
  * the enumeration values.
  */
inline def toJsonEnum(e: Enumeration): ToJSON[e.Value] = EnumerationInstances.toJsonEnum(e)

/** Creates a FromJSON instance for an Enumeration type that encodes the `toString` representations
  * of the enumeration values.
  */
inline def fromJsonEnum(e: Enumeration): FromJSON[e.Value] = EnumerationInstances.fromJsonEnum(e)

// This can be used instead of deriveJSON
inline def jsonEnum(e: Enumeration): JSON[e.Value] = EnumerationInstances.jsonEnum(e)

/** One subtype of a type switch: the formatters it contributes, already resolved.
  *
  * If `A` is itself a trait, the selector carries that trait's whole switch, so a parent switch
  * cannot take it apart again — `TypeSelectorContainer.typeSelectors` therefore has one element per
  * `sub`, not one per leaf subtype (this differs from Scala 2).
  */
final case class TypeSelector[A](toFormatters: ToFormatters, fromFormatters: FromFormatters)
final case class TypeSelectorToJSON[A](toFormatters: ToFormatters)
final case class TypeSelectorFromJSON[A](fromFormatters: FromFormatters)

trait TypeSelectorContainer {
  def typeSelectors: List[TypeSelector[?]]
}

/** Builds the selector for the subtype `A` of a `jsonTypeSwitch`. */
inline def sub[A]: TypeSelector[A] =
  TypeSelector(subTo[A].toFormatters, subFrom[A].fromFormatters)

/** Builds the write-side selector for the subtype `A` of a `toJsonTypeSwitch`. */
inline def subTo[A]: TypeSelectorToJSON[A] = {
  val traitMetaData = AnnotationReader.readTraitMetaData[A]
  val formatter = summonInline[ToJSON[A]].asInstanceOf[ToJSON[Any]]

  val (formatterByClass, serializedNamesByClass) =
    if (traitMetaData.isTrait)
      (formatter.toFormatters.formatterByClass, formatter.toFormatters.serializedNamesByClass)
    else {
      val clazz = summonInline[ClassTag[A]].runtimeClass
      (Map(clazz -> formatter), Map(clazz -> traitMetaData.top.serializedName))
    }

  TypeSelectorToJSON(
    ToFormatters(
      serializedNamesByClass = serializedNamesByClass,
      formatterByClass = formatterByClass,
      typeDiscriminator = traitMetaData.typeDiscriminator
    ))
}

/** Builds the read-side selector for the subtype `A` of a `fromJsonTypeSwitch`. */
inline def subFrom[A]: TypeSelectorFromJSON[A] = {
  val traitMetaData = AnnotationReader.readTraitMetaData[A]
  val formatter = summonInline[FromJSON[A]].asInstanceOf[FromJSON[Any]]

  val (formatterBySerializedName, serializedNames) =
    if (traitMetaData.isTrait)
      (formatter.fromFormatters.formatterBySerializedName, formatter.fromFormatters.serializedNames)
    else
      (Map(traitMetaData.top.serializedName -> formatter), Vector(traitMetaData.top.serializedName))

  TypeSelectorFromJSON(
    FromFormatters(
      serializedNames = serializedNames,
      formatterBySerializedName = formatterBySerializedName,
      typeDiscriminator = traitMetaData.typeDiscriminator
    ))
}

/** Creates a `JSON[T]` instance for some supertype `T`. The instance acts as a type-switch for the
  * given subtype selectors, delegating to their respective JSON instances based on a field that
  * acts as a type hint.
  */
inline def jsonTypeSwitch[T](
    selectors: List[TypeSelector[?]]): JSON[T] with TypeSelectorContainer = {
  require(selectors.nonEmpty, "jsonTypeSwitch needs at least one subtype")
  val discriminator = AnnotationReader.readTraitMetaData[T].typeDiscriminator
  typeSwitchInstance[T](selectors, discriminator)
}

private def typeSwitchInstance[T](
    selectors: List[TypeSelector[?]],
    discriminator: String): JSON[T] with TypeSelectorContainer = {
  val mergedTo = mergeTo(selectors.map(_.toFormatters), discriminator)
  val mergedFrom = mergeFrom(selectors.map(_.fromFormatters), discriminator)

  val toJson = JSONTypeSwitch.toJsonTypeSwitch[T](mergedTo)
  val fromJson = JSONTypeSwitch.fromJsonTypeSwitch[T](mergedFrom)

  new JSON[T] with TypeSelectorContainer {
    override def read(jval: JValue): JValidation[T] = fromJson.read(jval)
    override def write(value: T): JValue = toJson.write(value)
    override val fields: Set[String] = fromJson.fields
    override def subTypeNames: List[String] = mergedFrom.serializedNames.toList
    override def subTypeName(clazz: Class[_]): Option[String] =
      mergedTo.serializedNamesByClass.get(clazz)
    override val fromFormatters: FromFormatters = mergedFrom
    override val toFormatters: ToFormatters = mergedTo
    override def typeSelectors: List[TypeSelector[?]] = selectors
  }
}

inline def toJsonTypeSwitch[T](selectors: List[TypeSelectorToJSON[?]]): ToJSON[T] = {
  require(selectors.nonEmpty, "toJsonTypeSwitch needs at least one subtype")
  val discriminator = AnnotationReader.readTraitMetaData[T].typeDiscriminator
  JSONTypeSwitch.toJsonTypeSwitch[T](mergeTo(selectors.map(_.toFormatters), discriminator))
}

inline def fromJsonTypeSwitch[T](selectors: List[TypeSelectorFromJSON[?]]): FromJSON[T] = {
  require(selectors.nonEmpty, "fromJsonTypeSwitch needs at least one subtype")
  val discriminator = AnnotationReader.readTraitMetaData[T].typeDiscriminator
  JSONTypeSwitch.fromJsonTypeSwitch[T](mergeFrom(selectors.map(_.fromFormatters), discriminator))
}

// The type discriminator field always comes from the top-level type, never from a subtype, so it
// is stamped after the merge — `reduce` would skip `merge` for a single subtype.
private def mergeTo(formatters: List[ToFormatters], discriminator: String): ToFormatters =
  formatters.reduce(ToFormatters.merge).copy(typeDiscriminator = discriminator)

private def mergeFrom(formatters: List[FromFormatters], discriminator: String): FromFormatters =
  formatters.reduce(FromFormatters.merge).copy(typeDiscriminator = discriminator)

/** Bridges a `Mirror`'s `MirroredElemTypes` to the selector list the switches take. Only needed by
  * `derived`; everything else passes a list directly.
  *
  * A macro rather than an inline recursion on purpose: recursing nests one expansion per subtype,
  * so it exhausts `-Xmax-inlines` (default 32) at ~24 subtypes — and reports it as a bogus "No
  * given instance of type FromJSON[C24]". Expanding the tuple here makes the `sub*` calls siblings,
  * so depth stops growing with the number of subtypes.
  */
private[generic] inline def subsTo[T <: Tuple]: List[TypeSelectorToJSON[?]] =
  ${ subsToImpl[T] }

private[generic] inline def subsFrom[T <: Tuple]: List[TypeSelectorFromJSON[?]] =
  ${ subsFromImpl[T] }

private def subsToImpl[T <: Tuple: Type](using Quotes): Expr[List[TypeSelectorToJSON[?]]] =
  Expr.ofList(tupleElemTypes[T].map { case '[t] => '{ subTo[t] } })

private def subsFromImpl[T <: Tuple: Type](using Quotes): Expr[List[TypeSelectorFromJSON[?]]] =
  Expr.ofList(tupleElemTypes[T].map { case '[t] => '{ subFrom[t] } })

private def tupleElemTypes[T: Type](using Quotes): List[Type[?]] =
  Type.of[T] match {
    case '[EmptyTuple] => Nil
    case '[head *: tail] => Type.of[head] :: tupleElemTypes[tail]
    case _ =>
      quotes.reflect.report.errorAndAbort("Expected a tuple of subtypes, got " + Type.show[T])
  }
