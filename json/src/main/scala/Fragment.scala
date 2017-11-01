package io.sphere.json

import java.time.YearMonth
import java.util.{Locale, UUID, Currency}

import _root_.io.sphere.util.Money
import cats.data.NonEmptyList
import org.joda.time.{LocalDate, DateTime, LocalTime}
import org.json4s.{JSet, JArray, JObject, JValue}

/** Type class for types that can retrieve a fragment (a part of the (JSON) object). If an array is encountered,
  * all child-nodes are returned.
  * Similar to a simple JSONPath expression (e.g. "mainActor.movies.title"). */
trait Fragment[A] {
  def getFragment(r: A, fieldNames: Seq[String]): Seq[_]
}

object Fragment {

  /** Primitive fields (like String or Int), the leaves of the JSON tree. */
  private def primitiveField[A](i: A, fieldNames: Seq[String]): Seq[_] =
    if (fieldNames.isEmpty) Seq(i) else Seq.empty

  private def primitiveField[A](fieldNames: Seq[String]): Boolean = fieldNames.isEmpty

  trait PrimitiveFragment[A] extends Fragment[A] {
    def getFragment(r: A, fieldNames: Seq[String]): Seq[_] = primitiveField(r, fieldNames)
  }

  implicit def optionGetFragment[A](implicit c: Fragment[A]): Fragment[Option[A]] = new Fragment[Option[A]] {
    def getFragment(opt: Option[A], fieldNames: Seq[String]): Seq[_] = opt match {
      case Some(a) if (fieldNames.isEmpty) ⇒ Seq(a)
      case _ ⇒ Seq.empty
    }
  }

  implicit def listGetFragment[A](implicit w: Fragment[A]): Fragment[List[A]] = new Fragment[List[A]] {
    def getFragment(l: List[A], fieldNames: Seq[String]): Seq[_] = l.toSeq.flatMap { i => w.getFragment(i, fieldNames) }
  }

  implicit def nonEmptyListGetFragment[A](implicit w: Fragment[A]): Fragment[NonEmptyList[A]] = new Fragment[NonEmptyList[A]] {
    def getFragment(l: NonEmptyList[A], fieldNames: Seq[String]): Seq[_] = l.toList.flatMap { i => w.getFragment(i, fieldNames) }
  }

  implicit def seqGetFragment[A](implicit w: Fragment[A]): Fragment[Seq[A]] = new Fragment[Seq[A]] {
    def getFragment(l: Seq[A], fieldNames: Seq[String]): Seq[_] = l.flatMap { i => w.getFragment(i, fieldNames) }
  }

  implicit def setGetFragment[A](implicit w: Fragment[A]): Fragment[Set[A]] = new Fragment[Set[A]] {
    def getFragment(l: Set[A], fieldNames: Seq[String]): Seq[_] = l.toList.flatMap { i => w.getFragment(i, fieldNames) }
  }

  implicit def vectorGetFragment[A](implicit w: Fragment[A]): Fragment[Vector[A]] = new Fragment[Vector[A]] {
    def getFragment(l: Vector[A], fieldNames: Seq[String]): Seq[_] = l.flatMap { i => w.getFragment(i, fieldNames) }
  }

  implicit val intGetFragment: Fragment[Int] = new PrimitiveFragment[Int] {}

  implicit val stringGetFragment: Fragment[String] = new PrimitiveFragment[String] {}

  implicit val bigIntGetFragment: Fragment[BigInt] = new PrimitiveFragment[BigInt] {}

  implicit val shortGetFragment: Fragment[Short] = new PrimitiveFragment[Short] {}

  implicit val longGetFragment: Fragment[Long] = new PrimitiveFragment[Long] {}

  implicit val floatGetFragment: Fragment[Float] = new PrimitiveFragment[Float] {}

  implicit val doubleGetFragment: Fragment[Double] = new PrimitiveFragment[Double] {}

  implicit val booleanGetFragment: Fragment[Boolean] = new PrimitiveFragment[Boolean] {}

  implicit def mapGetFragment[A: Fragment](implicit w: Fragment[A]): Fragment[Map[String, A]] = new Fragment[Map[String, A]] {
    def getFragment(m: Map[String, A], fieldNames: Seq[String]) = fieldNames match {
      case Nil => Seq(m)
      case fieldName :: fieldNames =>
        m.get(fieldName).map(v => w.getFragment(v, fieldNames)).getOrElse(Seq.empty)
    }
  }

  implicit val moneyGetFragment: Fragment[Money] = new Fragment[Money] {
    def getFragment(m: Money, fieldNames: Seq[String]): Seq[_] =
      fieldNames match {
        case Nil => Seq(m)
        case "currencyCode" :: Nil => Seq(m.currency.getCurrencyCode)
        case "centAmount" :: Nil => Seq(m.centAmount)
        case _ => Seq.empty
      }
  }

  implicit val currencyGetFragment: Fragment[Currency] = new PrimitiveFragment[Currency] {}

  implicit val jValueGetFragment: Fragment[JValue] = new Fragment[JValue] {
    def getFragment(jval: JValue, fieldNames: Seq[String]): Seq[_] = jval match {
      case obj: JObject => jObjectGetFragment.getFragment(obj, fieldNames)
      case arr: JArray => listGetFragment[JValue].getFragment(arr.arr, fieldNames)
      case set: JSet   => setGetFragment[JValue].getFragment(set.set, fieldNames)
      case _ => primitiveField(jval.values, fieldNames)
    }
  }

  implicit val jObjectGetFragment: Fragment[JObject] = new Fragment[JObject] {
    def getFragment(jObj: JObject, fieldNames: Seq[String]): Seq[_] = fieldNames match {
      case Nil => Seq(jObj)
      case fieldName :: fieldNames =>
        jObj.findField(_._1 == fieldName).map { field =>
          jValueGetFragment.getFragment(field._2, fieldNames)
        }.getOrElse(Seq.empty)
    }
  }

  implicit val unitGetFragment: Fragment[Unit] = new PrimitiveFragment[Unit] {}

  implicit val dateTimeGetFragment: Fragment[DateTime] = new PrimitiveFragment[DateTime] {}

  implicit val timeGetFragment: Fragment[LocalTime] = new PrimitiveFragment[LocalTime] {}

  implicit val dateGetFragment: Fragment[LocalDate] = new PrimitiveFragment[LocalDate] {}

  implicit val yearMonthGetFragment: Fragment[YearMonth] = new PrimitiveFragment[YearMonth] {}

  implicit val uuidGetFragment: Fragment[UUID] = new PrimitiveFragment[UUID] {}

  implicit val localeGetFragment: Fragment[Locale] = new PrimitiveFragment[Locale] {}

  implicit def eitherGetFragment[A: Fragment, B: Fragment](implicit w: Fragment[A], v: Fragment[B]): Fragment[Either[A, B]] = new Fragment[Either[A, B]] {
    def getFragment(e: Either[A, B], fieldNames: Seq[String]): Seq[_] = e match {
      case Left(l) => w.getFragment(l, fieldNames)
      case Right(r) => v.getFragment(r, fieldNames)
    }
  }
}
