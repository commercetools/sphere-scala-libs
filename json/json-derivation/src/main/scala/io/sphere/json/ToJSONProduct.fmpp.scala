package io.sphere.json

import org.json4s._

object ToJSONProduct {

  /** Writes `"name":value`, or nothing at all if the value is absent. Returns whether the
    * enclosing object has any field yet, so the caller knows about the separating comma. */
  private def fld[A](s: JsonSink, name: String, v: A, wrote: Boolean)(implicit w: ToJSON[A]): Boolean =
    if (w.writesNothing(v)) wrote
    else {
      if (wrote) s.ch(',')
      s.string(name)
      s.ch(':')
      w.writeTo(v, s)
      true
    }

  def forProduct1[A, A1 : ToJSON](
    f: A => (String, A1)
  ): ToJSON[A] = new ToJSON.Always[A] {
    override def write(a: A): JValue = {
      val t = f(a)
      JObject((t._1 -> toJValue(t._2)) :: Nil)
    }
    override def writeTo(a: A, s: JsonSink): Unit = {
      val t = f(a)
      s.ch('{')
      fld(s, t._1, t._2, false)
      s.ch('}')
    }
  }

  <#list 2..22 as i>
  <#assign implTypeParams><#list 1..i as j>A${j} : ToJSON<#if i !=j>,</#if></#list></#assign>
  def forProduct${i}[A, ${implTypeParams}](
    f: A => (<#list 1..i as j>(String, A${j})<#if i !=j>, </#if></#list>)
  ): ToJSON[A] = new ToJSON.Always[A] {
    override def write(a: A): JValue = {
      val t = f(a)
      JObject(
        <#list 1..i as j>t._${j}._1 -> toJValue(t._${j}._2) :: </#list>Nil
      )
    }
    override def writeTo(a: A, s: JsonSink): Unit = {
      val t = f(a)
      s.ch('{')
      var wrote = false
      <#list 1..i as j>
      wrote = fld(s, t._${j}._1, t._${j}._2, wrote)
      </#list>
      s.ch('}')
    }
  }
  </#list>
}
