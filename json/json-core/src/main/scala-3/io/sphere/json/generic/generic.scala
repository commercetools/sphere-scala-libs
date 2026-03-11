package io.sphere.json.generic

import io.sphere.json.*
import io.sphere.json.generic.JSONTypeSwitch.{FromFormatters, ToFormatters}
import org.json4s.JsonAST.JValue

import scala.deriving.Mirror

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

inline def jsonTypeSwitch[SuperType, SubTypes <: Tuple]: JSON[SuperType] =
  JSONTypeSwitch.jsonTypeSwitch[SuperType, SubTypes]

inline def toJsonTypeSwitch[SuperType, SubTypes <: Tuple]: ToJSON[SuperType] = {
  val f = JSONTypeSwitch.deriveToFormatters[SuperType, SubTypes]
  JSONTypeSwitch.toJsonTypeSwitch[SuperType](f)
}

inline def fromJsonTypeSwitch[SuperType, SubTypes <: Tuple]: FromJSON[SuperType] = {
  val f = JSONTypeSwitch.deriveFromFormatters[SuperType, SubTypes]
  JSONTypeSwitch.fromJsonTypeSwitch[SuperType](f)
}

// Compatibility with the scala-2 methods, that will be deprecated later
// fromJsonTypeSwitch is not used, so no old syntax support will be added for now
// Compatibility with Scala 2 syntax
case class TypeSelector(clazz: Class[?], typeValue: String, json: JSON[?])

// Compatibility with Scala 2 syntax
trait TypeSelectorContainer {
  def typeSelectors: List[TypeSelector]
}

// Compatibility with Scala 2 syntax
private def addTypeSelectorContainer[A](derviedJson: JSON[A])(
    typeSelectors: List[TypeSelector]): JSON[A] with TypeSelectorContainer = {
  val additionalJsons = typeSelectors.map(_.json)
  val mergedFromFormatters = additionalJsons
    .map(_.fromFormatters)
    .fold(derviedJson.fromFormatters)(
      FromFormatters.merge(derviedJson.fromFormatters.typeDiscriminator))
  val mergedToFormatters = additionalJsons
    .map(_.toFormatters)
    .fold(derviedJson.toFormatters)(ToFormatters.merge(derviedJson.toFormatters.typeDiscriminator))

  // We create the write/read methods again with the all the formatters present, so they have the correct references
  val toJson = JSONTypeSwitch.toJsonTypeSwitch[A](mergedToFormatters)
  val fromJson = JSONTypeSwitch.fromJsonTypeSwitch[A](mergedFromFormatters)

  new JSON[A] with TypeSelectorContainer {
    override def read(jval: JValue): JValidation[A] = fromJson.read(jval)
    override def write(value: A): JValue = toJson.write(value)
    override val fields: Set[String] = fromJson.fields
    override def subTypeNames: Vector[String] = Vector.empty
    override val fromFormatters: FromFormatters = mergedFromFormatters
    override val toFormatters: ToFormatters = mergedToFormatters

    override def typeSelectors: List[TypeSelector] =
      toFormatters.serializedNamesByClass
        .map((cls, serializedName) => TypeSelector(cls, serializedName, this))
        .toList
  }
}

// jsonTypeSwitch is used up to 26 parameters, so I'll up to 28
// format: off
inline def jsonTypeSwitch[SuperType, A1: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, Tuple1[A1]])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON, A26: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON, A26: JSON, A27: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27)])(typeSelectors)
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON, A26: JSON, A27: JSON, A28: JSON](typeSelectors: List[TypeSelector]): JSON[SuperType] with TypeSelectorContainer =
  addTypeSelectorContainer(jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28)])(typeSelectors)
// format: on

// toJsonTypeSwitch is used up to 5 parameters
// format: off
inline def toJsonTypeSwitch[SuperType, A1: ToJSON](ignoredList: List[Nothing]): ToJSON[SuperType] =
  toJsonTypeSwitch[SuperType, Tuple1[A1]]
inline def toJsonTypeSwitch[SuperType, A1: ToJSON, A2: ToJSON](ignoredList: List[Nothing]): ToJSON[SuperType] =
  toJsonTypeSwitch[SuperType, (A1, A2)]
inline def toJsonTypeSwitch[SuperType, A1: ToJSON, A2: ToJSON, A3: ToJSON](ignoredList: List[Nothing]): ToJSON[SuperType] =
  toJsonTypeSwitch[SuperType, (A1, A2, A3)]
inline def toJsonTypeSwitch[SuperType, A1: ToJSON, A2: ToJSON, A3: ToJSON, A4: ToJSON](ignoredList: List[Nothing]): ToJSON[SuperType] =
  toJsonTypeSwitch[SuperType, (A1, A2, A3, A4)]
inline def toJsonTypeSwitch[SuperType, A1: ToJSON, A2: ToJSON, A3: ToJSON, A4: ToJSON, A5: ToJSON](ignoredList: List[Nothing]): ToJSON[SuperType] =
  toJsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5)]
// format: on

// fromJsonTypeSwitch is used up to 4 parameters (only in tests)
// format: off
inline def fromJsonTypeSwitch[SuperType, A1: FromJSON](ignoredList: List[Nothing]): FromJSON[SuperType] =
  fromJsonTypeSwitch[SuperType, Tuple1[A1]]
inline def fromJsonTypeSwitch[SuperType, A1: FromJSON, A2: FromJSON](ignoredList: List[Nothing]): FromJSON[SuperType] =
  fromJsonTypeSwitch[SuperType, (A1, A2)]
inline def fromJsonTypeSwitch[SuperType, A1: FromJSON, A2: FromJSON, A3: FromJSON](ignoredList: List[Nothing]): FromJSON[SuperType] =
  fromJsonTypeSwitch[SuperType, (A1, A2, A3)]
inline def fromJsonTypeSwitch[SuperType, A1: FromJSON, A2: FromJSON, A3: FromJSON, A4: FromJSON](ignoredList: List[Nothing]): FromJSON[SuperType] =
  fromJsonTypeSwitch[SuperType, (A1, A2, A3, A4)]
inline def fromJsonTypeSwitch[SuperType, A1: FromJSON, A2: FromJSON, A3: FromJSON, A4: FromJSON, A5: FromJSON](ignoredList: List[Nothing]): FromJSON[SuperType] =
  fromJsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5)]
// format: on
