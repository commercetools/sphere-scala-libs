package io.sphere.json.generic

import io.sphere.json.*

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

inline def jsonTypeSwitch[SuperType, SubTypes <: Tuple](): JSON[SuperType] =
  JSONTypeSwitch.jsonTypeSwitch[SuperType, SubTypes]()

inline def toJsonTypeSwitch[SuperType, SubTypes <: Tuple]: ToJSON[SuperType] = {
  val info = JSONTypeSwitch.readTraitInformation[SuperType, SubTypes]
  JSONTypeSwitch.toJsonTypeSwitch[SuperType](info)
}

inline def fromJsonTypeSwitch[SuperType, SubTypes <: Tuple]: FromJSON[SuperType] = {
  val info = JSONTypeSwitch.readTraitInformation[SuperType, SubTypes]
  JSONTypeSwitch.fromJsonTypeSwitch[SuperType](info)
}
