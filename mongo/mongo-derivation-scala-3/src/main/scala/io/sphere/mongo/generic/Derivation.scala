//package io.sphere.mongo.generic
//
//import io.sphere.mongo.format.MongoFormat
//
//import scala.deriving.Mirror
//
//inline given derived[A](using Mirror.Of[A]): MongoFormat[A] = Derivation.derived
//
//private object Derivation:
//  inline def derived[A](using m: Mirror.Of[A]): MongoFormat[A] =
//    inline m match
//      case s: Mirror.SumOf[A] => deriveSum(s)
//      case p: Mirror.ProductOf[A] => deriveProduct(p)
//
//  inline def deriveSum[A](s: Mirror.SumOf[A]): MongoFormat[A] = dummyFormat[A]
//
//  inline def deriveProduct[A](p: Mirror.ProductOf[A]): MongoFormat[A] =  new MongoFormat[A]:
//    val instances = summonAll[p.MirroredElemTypes].iterator
//    override def toMongoValue(a: A): Any =
//      val fields = a.asInstanceOf[Product].productIterator
//      val fieldNames = a.asInstanceOf[Product].productElementNames
//      println(s"-- ${instances.zip(fields).zip(fieldNames)}")
//      ???
//
//    override def fromMongoValue(any: Any): A =
//      p.fromTuple()
//      ???
//
//
//  inline def summonAll[T <: Tuple]: Vector[MongoFormat[Any]] =
//    import scala.compiletime.{erasedValue, summonInline}
//    inline erasedValue[T] match
//      case _: EmptyTuple => Vector.empty
//      case _: (t *: ts) => summonInline[MongoFormat[t]].asInstanceOf[MongoFormat[Any]] +: summonAll[ts]
//

package io.sphere.mongo.generic

import scala.deriving.Mirror

type FakeBson = Map[String, Any] | SingleValue | String

trait FakeMongoFormat[A]:
  def toFakeBson(a: A): FakeBson
  def fromFakeBson(bson: FakeBson): A

case class SingleValue(value: Any)

object FakeMongoFormat:
  inline def apply[A: FakeMongoFormat]: FakeMongoFormat[A] = summon

  inline given derive[A](using Mirror.Of[A]): FakeMongoFormat[A] = Derivation.derived

  given FakeMongoFormat[Int] = new FakeMongoFormat[Int]:
    override def toFakeBson(a: Int): FakeBson = SingleValue(a)
    override def fromFakeBson(bson: FakeBson): Int =
      bson match
       case SingleValue(int: Int) => int
       case _ => throw new Exception("not an int")

  given FakeMongoFormat[String] = new FakeMongoFormat[String]:
    override def toFakeBson(a: String): FakeBson = SingleValue(a)
    override def fromFakeBson(bson: FakeBson): String =
      bson match
        case SingleValue(str: String) => str
        case _ => throw new Exception("not a String")

  given FakeMongoFormat[Boolean] = new FakeMongoFormat[Boolean]:
    override def toFakeBson(a: Boolean): FakeBson = SingleValue(a)
    override def fromFakeBson(bson: FakeBson): Boolean =
      bson match
        case SingleValue(bool: Boolean) => bool
        case _ => throw new Exception("not a Boolean")

  private object Derivation:
    import scala.compiletime.{constValue, constValueTuple, erasedValue, summonInline}

    inline def derived[A](using m: Mirror.Of[A]): FakeMongoFormat[A] =
      inline m match
        case s: Mirror.SumOf[A] => deriveSum(s)
        case p: Mirror.ProductOf[A] => deriveProduct(p)

    inline private def deriveSum[A](mirrorOfSum: Mirror.SumOf[A]): FakeMongoFormat[A] = new FakeMongoFormat[A]:
      val typeField = "typeDiscriminator"
      val formatters = summonFormatters[mirrorOfSum.MirroredElemTypes]
      val names = constValueTuple[mirrorOfSum.MirroredElemLabels].productIterator.toVector.asInstanceOf[Vector[String]]
      val formattersByTypeName = names.zip(formatters).toMap

      // println(s"sum names $names")

      override def toFakeBson(a: A): FakeBson =
        // we never get a trait here, only classes
        val typeName = a.asInstanceOf[Product].productPrefix
        val map = formattersByTypeName(typeName).toFakeBson(a).asInstanceOf[Map[String, Any]]
        map + (typeField -> typeName)

      override def fromFakeBson(bson: FakeBson): A =
        bson match
          case map: Map[String, _] =>
            val typeName = map(typeField).asInstanceOf[String]
            formattersByTypeName(typeName).fromFakeBson(map).asInstanceOf[A]
          case _ => throw new Exception("not a Map")

    end deriveSum

    inline def deriveProduct[A](mirrorOfProduct: Mirror.ProductOf[A]): FakeMongoFormat[A] =  new FakeMongoFormat[A]:
      val formatters = summonFormatters[mirrorOfProduct.MirroredElemTypes]
      val fieldNames = constValueTuple[mirrorOfProduct.MirroredElemLabels].productIterator.toVector.asInstanceOf[Vector[String]]

      override def toFakeBson(a: A): FakeBson =
        val values = a.asInstanceOf[Product].productIterator
        formatters.zip(values).zip(fieldNames).map {
          case ((format, value), fieldName) =>
            fieldName -> format.toFakeBson(value)
        }.toMap

      override def fromFakeBson(bson: FakeBson): A =
        bson match
          case map: Map[String, _] @unchecked =>
            val res = fieldNames.zip(formatters).map((fn, format) => format.fromFakeBson(map(fn).asInstanceOf[FakeBson]))
            val tuple = Tuple.fromArray(res.toArray)
            mirrorOfProduct.fromTuple(tuple.asInstanceOf[mirrorOfProduct.MirroredElemTypes])

          case _ => throw new Exception("not a Map")
    end deriveProduct

    inline private def summonFormatters[T <: Tuple]: Vector[FakeMongoFormat[Any]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Vector.empty
        case _: (t *: ts) => summonInline[FakeMongoFormat[t]].asInstanceOf[FakeMongoFormat[Any]] +: summonFormatters[ts]



