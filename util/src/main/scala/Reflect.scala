package io.sphere.util

import org.json4s.scalap.scalasig._

object Reflect extends Logging {
  case class CaseClassMeta(fields: IndexedSeq[CaseClassFieldMeta])
  case class CaseClassFieldMeta(name: String, default: Option[Any] = None)

  /** Obtains minimal meta information about a case class or object via scalap.
    * The meta information contains a list of names and default values which
    * represent the arguments of the case class constructor and their default values,
    * in the order they are defined.
    *
    * Note: Does not work for case classes or objects nested in other classes or traits
    *       (nesting inside other objects is fine).
    * Note: Only a single default value is obtained for each field. Thus avoid default
    *       values that are different on each invocation (e.g. new DateTime()). In other words,
    *       the case class constructors should be pure functions.
    */
  val getCaseClassMeta = new Memoizer[Class[_], CaseClassMeta](clazz => {
    logger.trace(
      "Initializing reflection metadata for case class or object %s".format(clazz.getName))
    CaseClassMeta(getCaseClassFieldMeta(clazz))
  })

  private def getCompanionClass(clazz: Class[_]): Class[_] =
    Class.forName(clazz.getName + "$", true, clazz.getClassLoader)
  private def getCompanionObject(companionClass: Class[_]): Object =
    companionClass.getField("MODULE$").get(null)
  private def getCaseClassFieldMeta(clazz: Class[_]): IndexedSeq[CaseClassFieldMeta] =
    if (clazz.getName.endsWith("$")) IndexedSeq.empty[CaseClassFieldMeta]
    else {
      val companionClass = getCompanionClass(clazz)
      val companionObject = getCompanionObject(companionClass)

      val maybeSym = clazz.getName.split("\\$") match {
        case Array(_) => ScalaSigParser.parse(clazz).flatMap(_.topLevelClasses.headOption)
        case Array(h, t @ _*) =>
          val name = t.last
          val topSymbol = ScalaSigParser.parse(Class.forName(h, true, clazz.getClassLoader))
          topSymbol.flatMap(_.symbols.collectFirst { case s: ClassSymbol if s.name == name => s })
      }

      val sym = maybeSym.getOrElse {
        throw new IllegalArgumentException(
          "Unable to find class symbol through ScalaSigParser for class %s."
            .format(clazz.getName))
      }

      sym.children.iterator
        .collect { case m: MethodSymbol if m.isCaseAccessor && !m.isPrivate => m }
        .zipWithIndex
        .map { case (ms, idx) =>
          val defaultValue =
            try Some(companionClass.getMethod("apply$default$" + (idx + 1)).invoke(companionObject))
            catch {
              case _: NoSuchMethodException => None
            }
          CaseClassFieldMeta(ms.name, defaultValue)
        }
        .toIndexedSeq
    }
}
