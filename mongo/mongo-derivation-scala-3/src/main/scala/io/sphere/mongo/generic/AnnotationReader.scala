package io.sphere.mongo.generic

import scala.quoted.{Expr, Quotes, Type, Varargs}

private type MA = MongoAnnotation

case class Field(
    rawName: String,
    embedded: Boolean,
    ignored: Boolean,
    mongoKey: Option[MongoKey],
    defaultArgument: Option[Any]) {
  val name: String = mongoKey.map(_.value).getOrElse(rawName)
}
case class CaseClassMetaData(
    name: String,
    typeHintRaw: Option[MongoTypeHint],
    fields: Vector[Field]
) {
  val typeHint: Option[String] =
    typeHintRaw.map(_.value).filterNot(_.toList.forall(_ == ' '))
}

case class TraitMetaData(
    top: CaseClassMetaData,
    typeHintFieldRaw: Option[MongoTypeHintField],
    subtypes: Map[String, CaseClassMetaData]
) {
  val typeDiscriminator: String = typeHintFieldRaw.map(_.value).getOrElse("type")
}

object AnnotationReader {

  def mongoEnum(e: Enumeration): TypedMongoFormat[e.Value] = new TypedMongoFormat[e.Value] {
    def toMongoValue(a: e.Value): MongoType = a.toString

    def fromMongoValue(any: MongoType): e.Value = e.withName(any.asInstanceOf[String])
  }

  inline def readTraitMetaData[T]: TraitMetaData = ${ readTraitMetaDataImpl[T] }

  inline def readCaseClassMetaData[T]: CaseClassMetaData = ${ readCaseClassMetaDataImpl[T] }

  private def readCaseClassMetaDataImpl[T: Type](using Quotes): Expr[CaseClassMetaData] =
    AnnotationReader().readCaseClassMetaData[T]

  private def readTraitMetaDataImpl[T: Type](using Quotes): Expr[TraitMetaData] =
    AnnotationReader().readTraitMetaData[T]
}

class AnnotationReader(using q: Quotes):
  import q.reflect.*

  def readCaseClassMetaData[T: Type]: Expr[CaseClassMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    caseClassMetaData(sym)
  }

  def readTraitMetaData[T: Type]: Expr[TraitMetaData] = {
    val sym = TypeRepr.of[T].typeSymbol
    val typeHintField =
      sym.annotations.map(findMongoTypeHintField).find(_.isDefined).flatten match {
        case Some(thf) => '{ Some($thf) }
        case None => '{ None }
      }

    '{
      TraitMetaData(
        top = ${ caseClassMetaData(sym) },
        typeHintFieldRaw = $typeHintField,
        subtypes = ${ subtypeAnnotations(sym) }
      )
    }
  }

  private def annotationTree(tree: Tree): Option[Expr[MA]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MA]).map(_.asExprOf[MA])

  private def findEmbedded(tree: Tree): Boolean =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MongoEmbedded]).isDefined

  private def findIgnored(tree: Tree): Boolean =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MongoIgnore]).isDefined

  private def findKey(tree: Tree): Option[Expr[MongoKey]] =
    Option.when(tree.isExpr)(tree.asExpr).filter(_.isExprOf[MongoKey]).map(_.asExprOf[MongoKey])

  private def findTypeHint(tree: Tree): Option[Expr[MongoTypeHint]] =
    Option
      .when(tree.isExpr)(tree.asExpr)
      .filter(_.isExprOf[MongoTypeHint])
      .map(_.asExprOf[MongoTypeHint])

  private def findMongoTypeHintField(tree: Tree): Option[Expr[MongoTypeHintField]] =
    Option
      .when(tree.isExpr)(tree.asExpr)
      .filter(_.isExprOf[MongoTypeHintField])
      .map(_.asExprOf[MongoTypeHintField])

  private def collectFieldInfo(companion: Symbol)(s: Symbol, paramIdx: Int): Expr[Field] =
    val embedded = Expr(s.annotations.exists(findEmbedded))
    val ignored = Expr(s.annotations.exists(findIgnored))
    val name = Expr(s.name)
    val mongoKey = s.annotations.map(findKey).find(_.isDefined).flatten match {
      case Some(k) => '{ Some($k) }
      case None => '{ None }
    }
    val defArgOpt = companion
      .methodMember(s"$$lessinit$$greater$$default$$${paramIdx + 1}")
      .headOption
      .map(dm => Ref(dm).asExprOf[Any]) match {
      case Some(k) => '{ Some($k) }
      case None => '{ None }
    }

    '{
      Field(
        rawName = $name,
        embedded = $embedded,
        ignored = $ignored,
        mongoKey = $mongoKey,
        defaultArgument = $defArgOpt)
    }

  private def caseClassMetaData(sym: Symbol): Expr[CaseClassMetaData] =
    val caseParams = sym.primaryConstructor.paramSymss.take(1).flatten
    val fields = Varargs(caseParams.zipWithIndex.map(collectFieldInfo(sym.companionModule)))
    val name = Expr(sym.name)
    val typeHint = sym.annotations.map(findTypeHint).find(_.isDefined).flatten match {
      case Some(th) => '{ Some($th) }
      case None => '{ None }
    }

    '{
      CaseClassMetaData(
        name = $name,
        typeHintRaw = $typeHint,
        fields = Vector($fields*)
      )
    }
  end caseClassMetaData

  private def subtypeAnnotation(sym: Symbol): Expr[(String, CaseClassMetaData)] =
    val name = Expr(sym.name)
    val annots = caseClassMetaData(sym)
    '{ ($name, $annots) }
  end subtypeAnnotation

  private def subtypeAnnotations(sym: Symbol): Expr[Map[String, CaseClassMetaData]] =
    val subtypes = Varargs(sym.children.map(subtypeAnnotation))
    '{ Map($subtypes*) }

end AnnotationReader
