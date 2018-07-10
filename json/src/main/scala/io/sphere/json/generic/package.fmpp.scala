package io.sphere.json


import cats.data.Validated.Valid
import cats.data.ValidatedNel
import cats.syntax.apply._
import cats.syntax.option._

import scala.annotation.meta.getter
import scala.collection.mutable.ListBuffer
import scala.language.experimental.macros
import scala.reflect.{ClassTag, classTag}
import io.sphere.util.{Memoizer, Reflect}
import org.json4s.JsonAST._
import org.json4s.jackson.compactJson
import org.json4s.JsonDSL._

/** The generic package provides generic functions for deriving JSON instances via
  * some runtime & compile-time reflection. */
package object generic extends GenericsJson {

  /** Creates a `ToJSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def toJsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: ToJSON, A2 <: T: ClassTag: ToJSON](selectors: List[TypeSelectorToJSON[_]]): ToJSON[T] with TypeSelectorToJSONContainer = {
    val inSelectors = typeSelectorToJSON[A1] :: typeSelectorToJSON[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorToJSONContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val writeMapBuilder = Map.newBuilder[Class[_], TypeSelectorToJSON[_]]

    allSelectors.foreach { s =>
      writeMapBuilder += (s.clazz -> s)
    }

    val writeMap = writeMapBuilder.result

    new ToJSON[T] with TypeSelectorToJSONContainer {
      override def typeSelectors: List[TypeSelectorToJSON[_]] = allSelectors

      def write(t: T): JValue = writeMap.get(t.getClass) match {
        case Some(ts) =>
          ts.write(t) match {
            case o @ JObject(obj) if obj.exists(_._1 == ts.typeField) => o
            case j: JObject => j ~ JField(ts.typeField, JString(ts.typeValue))
            case j => throw new IllegalStateException("The json is not an object but a " + j.getClass)
          }

        case None => throw new IllegalStateException("Can't find a serializer for a class " + t.getClass)
      }
    }
  }

  /** Creates a `FromJSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def fromJsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: FromJSON, A2 <: T: ClassTag: FromJSON](selectors: List[TypeSelectorFromJSON[_]]): FromJSON[T] with TypeSelectorFromJSONContainer = {
    val inSelectors = typeSelectorFromJSON[A1] :: typeSelectorFromJSON[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.jsonr match {
      case container: TypeSelectorFromJSONContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val readMapBuilder = Map.newBuilder[String, TypeSelectorFromJSON[_]]

    allSelectors.foreach { s =>
      readMapBuilder += (s.typeValue -> s)
    }

    val readMap = readMapBuilder.result
    val clazz = classTag[T].runtimeClass

    val typeField = Option(clazz.getAnnotation(classOf[JSONTypeHintField])) match {
      case Some(a) => a.value
      case None => defaultTypeFieldName
    }

    new FromJSON[T] with TypeSelectorFromJSONContainer {
      override def typeSelectors: List[TypeSelectorFromJSON[_]] = allSelectors

      def read(jval: JValue): ValidatedNel[JSONError, T] = jval match {
        case o: JObject =>
          findTypeValue(o, typeField) match {
            case Some(t) => readMap.get(t) match {
              case Some(ts) => ts.read(o).asInstanceOf[ValidatedNel[JSONError, T]]
              case None => jsonParseError("Invalid type value '" + t + "' in '%s'".format(compactJson(o)))
            }
            case None => jsonParseError("Missing type field '" + typeField + "' in '%s'".format(compactJson(o)))
          }
        case _ => jsonParseError("JSON object expected.")
      }
    }
  }


  /** Creates a `JSON[T]` instance for some supertype `T`. The instance acts as a type-switch
    * for the subtypes `A1` and `A2`, delegating to their respective JSON instances based
    * on a field that acts as a type hint. */
  def jsonTypeSwitch[T: ClassTag, A1 <: T: ClassTag: FromJSON: ToJSON, A2 <: T: ClassTag: FromJSON: ToJSON](selectors: List[TypeSelector[_]]): JSON[T] with TypeSelectorContainer = {
    val inSelectors = typeSelector[A1] :: typeSelector[A2] :: selectors
    val allSelectors = inSelectors.flatMap(s => s.serializer match {
      case container: TypeSelectorContainer => container.typeSelectors :+ s
      case _ => s :: Nil
    })

    val toJSON = toJsonTypeSwitch[T, A1, A2](selectors)
    val fromJSON = fromJsonTypeSwitch[T, A1, A2](selectors)

    new JSON[T] with TypeSelectorContainer {
      override def typeSelectors: List[TypeSelector[_]] = allSelectors

      def read(jval: JValue): ValidatedNel[JSONError, T] = fromJSON.read(jval)

      def write(t: T): JValue = toJSON.write(t)
    }
  }

  <#list 3..84 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def toJsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelectorToJSON[_]]): ToJSON[T] with TypeSelectorToJSONContainer = toJsonTypeSwitch[T, ${typeParams}](typeSelectorToJSON[A${i}] :: selectors)
  </#list>

  <#list 3..84 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def fromJsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelectorFromJSON[_]]): FromJSON[T] with TypeSelectorFromJSONContainer = fromJsonTypeSwitch[T, ${typeParams}](typeSelectorFromJSON[A${i}] :: selectors)
  </#list>

  <#list 3..84 as i>
  <#assign typeParams><#list 1..i-1 as j>A${j}<#if i-1 != j>,</#if></#list></#assign>
  <#assign implTypeParams><#list 1..i as j>A${j} <: T : FromJSON : ToJSON : ClassTag<#if i !=j>,</#if></#list></#assign>
  def jsonTypeSwitch[T: ClassTag, ${implTypeParams}](selectors: List[TypeSelector[_]]): JSON[T] with TypeSelectorContainer = jsonTypeSwitch[T, ${typeParams}](typeSelector[A${i}] :: selectors)
  </#list>

}
