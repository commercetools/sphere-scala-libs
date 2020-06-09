package json

import java.util.UUID

import io.sphere.json._
import io.sphere.json.generic._
import io.sphere.mongo.generic._
import io.sphere.mongo.format.MongoFormat
import io.sphere.mongo.format.DefaultMongoFormats._
import io.sphere.mongo.format._
import io.sphere.util.BaseMoney
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

case class Reference(typeId: String, id: UUID)

object Reference {
  implicit val json: JSON[Reference] = jsonProduct(apply _)
  implicit val mongoFormat: MongoFormat[Reference] = mongoProduct(apply _)
}

case class Price(id: String, value: BaseMoney, validUntil: DateTime)

object Price {
  // the lib does not ship a `MongoFormat[DateTime]`
  implicit val dateTimeAsIsoStringFormat: MongoFormat[DateTime] = new MongoFormat[DateTime] {
    override def toMongoValue(dt: DateTime): Any = ISODateTimeFormat.dateTime.print(dt.withZone(DateTimeZone.UTC))
    override def fromMongoValue(any: Any): DateTime = any match {
      case s: String => new DateTime(s, DateTimeZone.UTC)
      case _ => sys.error("String expected")
    }
  }

  implicit val json: JSON[Price] = jsonProduct(apply _)
  implicit val mongoFormat: MongoFormat[Price] = mongoProduct(apply _)
}

case class ProductVariant(id: Long, prices: Vector[Price], attributes: Map[String, String])

object ProductVariant {
  implicit val json: JSON[ProductVariant] = jsonProduct(apply _)
  implicit val mongoFormat: MongoFormat[ProductVariant] = mongoProduct(apply _)
}

case class Product(
  id: UUID,
  version: Long,
  productType: Reference,
  variants: Vector[ProductVariant])

object Product {
  implicit val json: JSON[Product] = jsonProduct(apply _)
  implicit val mongoFormat: MongoFormat[Product] = mongoProduct(apply _)
}

object JsonBenchmark {

  val lotsOfIntsList = Range(1, 100000).toList
  val lotsOfIntsSeq = Range(1, 100000).toSeq
  val lotsOfIntsVector = Range(1, 100000).toVector
  val lotsOfIntsAsJson = Range(1, 100000).mkString("[", ",", "]")
  val lotsOfIntsMongoValue = toMongo(lotsOfIntsVector)
  val bigMap: Map[String, String] = lotsOfIntsList.map(i => s"key$i" -> s"value$i").toMap
  val bigMapMongoValue = toMongo(bigMap)

  val prices = for (i <- 1 to 200) yield
    s"""
       |{
       |  "id": "$i",
       |  "value": {
       |    "centAmount": $i,
       |    "currencyCode": "USD"
       |  },
       |  "validUntil": "2025-12-14T12:50:25.070Z"
       |}
       """.stripMargin

  val customAttributes =
    (for (i <- 1 to 80) yield s""" "field-$i": "value $i" """).mkString("{", ",", "}")


  val variants = for (i <- 1 to 100) yield
    s"""{
        |  "id": $i,
        |  "prices": ${prices.mkString("[", ",", "]")},
        |  "images": [],
        |  "attributes": $customAttributes,
        |  "categories":[]
        |}""".stripMargin

  val json =
    s"""{
        |  "id": "ff30b141-67e4-41bb-97c5-4121c42d602a",
        |  "version": 2,
        |  "productType": {
        |    "typeId": "product-type",
        |    "id": "5a4c142a-40b8-4b86-b944-2259d39ced22"
        |  },
        |  "name": {"de-DE":"Ein Product 1","en":"Some Product 1"},
        |  "categories":[],
        |  "categoryOrderHints":{},
        |  "slug": {"en":"product_slug_1_4ff4aaa3-2dc9-4aca-8db9-1c68a341de13"},
        |  "variants": ${variants.mkString("[", ",",  "]")},
        |  "searchKeywords":{},
        |  "hasStagedChanges":false,
        |  "published":true,
        |  "createdAt":"2015-12-14T12:50:23.679Z",
        |  "lastModifiedAt":"2015-12-14T12:50:25.070Z"
        |}
      """.stripMargin

  val product = getFromJSON[Product](json)
  val productMongoValue = toMongo(product)
}
