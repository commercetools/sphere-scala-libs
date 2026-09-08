package io.sphere.json.generic

import io.sphere.json.*
import io.sphere.json.generic.JSONTypeSwitch.{FromFormatters, ToFormatters}
import io.sphere.util.TraitMetaData
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

/** Builds the selector for the subtype `A` of a `jsonTypeSwitch`.
  *
  * The three `sub*` methods are inline only to summon `A`'s instances and read its serialized name;
  * all the work happens in the plain `*Selector` defs below. That keeps what a `sub[A]` adds to its
  * call site down to a handful of instructions, so a list of hundreds of subtypes — or a `derived`
  * for a sealed trait with hundreds of children — stays well under the JVM's 64KB method limit.
  */
inline def sub[A]: TypeSelector[A] =
  selector(
    AnnotationReader.readSerializedName[A],
    summonInline[ClassTag[A]],
    summonInline[ToJSON[A]],
    summonInline[FromJSON[A]])

/** Builds the write-side selector for the subtype `A` of a `toJsonTypeSwitch`. */
inline def subTo[A]: TypeSelectorToJSON[A] =
  toSelector(
    AnnotationReader.readSerializedName[A],
    summonInline[ClassTag[A]],
    summonInline[ToJSON[A]])

/** Builds the read-side selector for the subtype `A` of a `fromJsonTypeSwitch`. */
inline def subFrom[A]: TypeSelectorFromJSON[A] =
  fromSelector(AnnotationReader.readSerializedName[A], summonInline[FromJSON[A]])

private def selector[A](
    serializedName: String,
    classTag: ClassTag[A],
    toJson: ToJSON[A],
    fromJson: FromJSON[A]): TypeSelector[A] =
  TypeSelector(
    toSelector(serializedName, classTag, toJson).toFormatters,
    fromSelector(serializedName, fromJson).fromFormatters)

private def toSelector[A](
    serializedName: String,
    classTag: ClassTag[A],
    formatter: ToJSON[A]): TypeSelectorToJSON[A] =
  TypeSelectorToJSON(
    // A non-null `toFormatters` means `A` is a trait: it brings its own switch's whole subtype
    // table, and must not be added as a single class itself.
    if (formatter.toFormatters != null) formatter.toFormatters
    else {
      val clazz = classTag.runtimeClass
      ToFormatters(
        serializedNamesByClass = Map(clazz -> serializedName),
        formatterByClass = Map(clazz -> formatter.asInstanceOf[ToJSON[Any]]),
        typeDiscriminator = overwrittenByTheSwitch
      )
    })

private def fromSelector[A](
    serializedName: String,
    formatter: FromJSON[A]): TypeSelectorFromJSON[A] =
  TypeSelectorFromJSON(
    if (formatter.fromFormatters != null) formatter.fromFormatters
    else
      FromFormatters(
        serializedNames = Vector(serializedName),
        formatterBySerializedName = Map(serializedName -> formatter.asInstanceOf[FromJSON[Any]]),
        typeDiscriminator = overwrittenByTheSwitch
      ))

/** A selector never decides the type discriminator — the switch stamps the top-level type's one on
  * the merged formatters, so whatever a selector carries is dropped.
  */
private val overwrittenByTheSwitch = TraitMetaData.defaultTypeDiscriminatorName

/** Creates a `JSON[T]` instance for some supertype `T`. The instance acts as a type-switch for the
  * given subtype selectors, delegating to their respective JSON instances based on a field that
  * acts as a type hint.
  */
inline def jsonTypeSwitch[T](
    selectors: List[TypeSelector[?]]): JSON[T] with TypeSelectorContainer = {
  require(selectors.nonEmpty, "jsonTypeSwitch needs at least one subtype")
  val discriminator = AnnotationReader.readTypeDiscriminator[T]
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
  val discriminator = AnnotationReader.readTypeDiscriminator[T]
  JSONTypeSwitch.toJsonTypeSwitch[T](mergeTo(selectors.map(_.toFormatters), discriminator))
}

inline def fromJsonTypeSwitch[T](selectors: List[TypeSelectorFromJSON[?]]): FromJSON[T] = {
  require(selectors.nonEmpty, "fromJsonTypeSwitch needs at least one subtype")
  val discriminator = AnnotationReader.readTypeDiscriminator[T]
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
