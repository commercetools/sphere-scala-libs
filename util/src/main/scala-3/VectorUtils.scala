package io.sphere.util

object VectorUtils {

  extension [A](vector: Vector[A]) {

    // toMap by default will remove all but one of the key value pairs in case of duplicate keys
    def toMapWithNoDuplicateKeys[K, V](using A <:< (K, V)): Map[K, V] = {
      val duplicateKeys =
        vector.groupBy(_._1).collect { case (key, values) if values.size >= 2 => key }
      if (duplicateKeys.nonEmpty)
        throw new Exception(
          s"Cannot construct Map because the following keys are duplicates: ${duplicateKeys.mkString(", ")}")
      else
        vector.toMap
    }
  }

}
