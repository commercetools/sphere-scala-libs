package json

import java.util.UUID

import org.json4s.StringInput
import org.json4s.jackson._
import org.openjdk.jmh.annotations.Benchmark
import io.sphere.json._
import io.sphere.json.generic._
import io.sphere.util.Money

class JsonBenchmark {

  /* on local mac
[info] Benchmark                                                                   Mode  Cnt         Score          Error   Units
[info] JsonBenchmark.parseFromStringToCaseClass                                   thrpt   10        29.521 ±        1.757   ops/s
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.alloc.rate                    thrpt   10      1821.340 ±      108.344  MB/sec
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.alloc.rate.norm               thrpt   10  64704842.660 ±      187.155    B/op
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.churn.PS_Eden_Space           thrpt   10      1816.530 ±     1021.302  MB/sec
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.churn.PS_Eden_Space.norm      thrpt   10  64855125.880 ± 37793011.733    B/op
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.churn.PS_Survivor_Space       thrpt   10         5.834 ±        6.969  MB/sec
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.churn.PS_Survivor_Space.norm  thrpt   10    207593.641 ±   248847.677    B/op
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.count                         thrpt   10        14.000                 counts
[info] JsonBenchmark.parseFromStringToCaseClass:·gc.time                          thrpt   10        56.000                     ms
[info] JsonBenchmark.parseFromStringToJValue                                      thrpt   10        68.756 ±        0.750   ops/s
[info] JsonBenchmark.parseFromStringToJValue:·gc.alloc.rate                       thrpt   10      1175.246 ±       12.823  MB/sec
[info] JsonBenchmark.parseFromStringToJValue:·gc.alloc.rate.norm                  thrpt   10  17926566.571 ±      227.600    B/op
[info] JsonBenchmark.parseFromStringToJValue:·gc.churn.PS_Eden_Space              thrpt   10      1155.652 ±      487.597  MB/sec
[info] JsonBenchmark.parseFromStringToJValue:·gc.churn.PS_Eden_Space.norm         thrpt   10  17632708.346 ±  7480605.032    B/op
[info] JsonBenchmark.parseFromStringToJValue:·gc.churn.PS_Survivor_Space          thrpt   10         1.951 ±        4.138  MB/sec
[info] JsonBenchmark.parseFromStringToJValue:·gc.churn.PS_Survivor_Space.norm     thrpt   10     29801.861 ±    63100.251    B/op
[info] JsonBenchmark.parseFromStringToJValue:·gc.count                            thrpt   10        12.000                 counts
[info] JsonBenchmark.parseFromStringToJValue:·gc.time                             thrpt   10        40.000                     ms   */

  @Benchmark
  def parseFromStringToJValue(): Unit = {
    val jvalue = parseJson(StringInput(JsonBenchmark.json))
    assert(jvalue != null)
  }

  @Benchmark
  def parseFromStringToCaseClass(): Unit = {
    val product = getFromJSON[Product](JsonBenchmark.json)
    assert(product.version == 2)
  }

}

case class Reference(
  typeId: String,
  id: UUID)
object Reference {
  implicit val json: JSON[Reference] = jsonProduct((apply _).curried)
}

case class Price(
  id: String,
  value: Money)
object Price {
  implicit val json: JSON[Price] = jsonProduct((apply _).curried)
}

case class ProductVariant(
  id: Long,
  prices: Vector[Price],
  attributes: Map[String, String])
object ProductVariant {
  implicit val json: JSON[ProductVariant] = jsonProduct((apply _).curried)
}

case class Product(
  id: UUID,
  version: Long,
  productType: Reference,
  variants: Vector[ProductVariant])
object Product {
  implicit val json: JSON[Product] = jsonProduct((apply _).curried)
}

object JsonBenchmark {
  val prices = for (i ← 1 to 200) yield
    s"""
       |{
       |  "id": "$i",
       |  "value": {
       |    "centAmount": $i,
       |    "currencyCode": "USD"
       |  }
       |}
       """.stripMargin

  val customAttributes =
    (for (i ← 1 to 80) yield s""" "field-$i": "value $i" """).mkString("{", ",", "}")


  val variants = for (i ← 1 to 100) yield
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
}