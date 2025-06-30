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

// jsonTypeSwitch is used up to 26 parameters, so I'll up to 28
// format: off
inline def jsonTypeSwitch[SuperType, A1: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, Tuple1[A1]]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON, A26: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON, A26: JSON, A27: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27)]
inline def jsonTypeSwitch[SuperType, A1: JSON, A2: JSON, A3: JSON, A4: JSON, A5: JSON, A6: JSON, A7: JSON, A8: JSON, A9: JSON, A10: JSON, A11: JSON, A12: JSON, A13: JSON, A14: JSON, A15: JSON, A16: JSON, A17: JSON, A18: JSON, A19: JSON, A20: JSON, A21: JSON, A22: JSON, A23: JSON, A24: JSON, A25: JSON, A26: JSON, A27: JSON, A28: JSON](ignoredList: List[Nothing]): JSON[SuperType] =
  jsonTypeSwitch[SuperType, (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25, A26, A27, A28)]
// format: on

// toJsonTypeSwitch is used up to 5 parameters, so I'll add up to 7
