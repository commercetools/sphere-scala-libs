package io.sphere.json

import org.json4s._

trait ToJSONAux {
  type B
  val toJSON: ToJSON[B]
  val typeValue: String
}

case class TypeName(value: String) extends AnyVal
object TypeName {
  def apply(clazz: Class[_]): TypeName = {
    val value = clazz.getName

    // Class.getName returns a different name if called on the type or on an instance:
    // - Type.getClass -> package.Type$
    // - instance.getClass -> package.Type
    // remove the last $ so that we can compare both
    if (value.endsWith("$")) TypeName(value.substring(0, value.length - 1))
    else TypeName(value)
  }
}

case class TypesSwitchToJSON[A](
    typeField: String = "type",
    // could use typeValue as key
    subTypes: Map[TypeName, ToJSONAux { type B <: A }]
) extends ToJSON[A] {

  override def write(value: A): JValue = {
    val typeName = TypeName(value.getClass)
    subTypes.get(typeName) match {
      case Some(leafTypeToJSON) =>
        leafTypeToJSON.toJSON.write(value.asInstanceOf[leafTypeToJSON.B]) match {
          case o @ JObject(obj) =>
            if (obj.exists(_._1 == typeField)) o
            else JObject(JField(typeField, JString(leafTypeToJSON.typeValue)) :: obj)

          case j => throw new IllegalStateException("The json is not an object but a " + j.getClass)
        }
      case None =>
        throw new IllegalStateException("Can't find a serializer for a class " + typeName.value)
    }
  }
}
