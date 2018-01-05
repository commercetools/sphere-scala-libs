package io.sphere.json

import org.json4s._

object ToJSONProduct {

  def forProduct1[A, A1 : ToJSON](
    f: A ⇒ (String, A1)
  ): ToJSON[A] = new ToJSON[A] {
    override def write(a: A): JValue = {
      val t = f(a)
      JObject(List(
        t._1 -> toJValue(t._2)
      ))
    }
  }

  <#list 2..22 as i>
  <#assign implTypeParams><#list 1..i as j>A${j} : ToJSON<#if i !=j>,</#if></#list></#assign>
  def forProduct${i}[A, ${implTypeParams}](
    f: A ⇒ (<#list 1..i as j>(String, A${j})<#if i !=j>, </#if></#list>)
  ): ToJSON[A] = new ToJSON[A] {
    override def write(a: A): JValue = {
      val t = f(a)
      JObject(
        <#list 1..i as j>t._${j}._1 -> toJValue(t._${j}._2) :: </#list>Nil
      )
    }
  }
  </#list>
}
