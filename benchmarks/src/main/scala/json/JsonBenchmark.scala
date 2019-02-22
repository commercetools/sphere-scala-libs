package json

import java.util.UUID

import io.sphere.json._
import io.sphere.json.generic._
import io.sphere.util.BaseMoney
import org.json4s.StringInput
import org.json4s.jackson._
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class JsonBenchmark {

  /* on local mac
  jmh:run

Benchmark                                  Mode  Cnt   Score    Error  Units
JsonBenchmark.listReader                  thrpt    5  65,248 ± 17,094  ops/s
JsonBenchmark.parseFromStringToCaseClass  thrpt    5  13,365 ±  0,467  ops/s
JsonBenchmark.parseFromStringToJValue     thrpt    5  84,705 ± 12,377  ops/s
JsonBenchmark.seqReader                   thrpt    5  64,475 ± 14,184  ops/s
JsonBenchmark.serializeCaseClassToString  thrpt    5  40,563 ±  4,731  ops/s
JsonBenchmark.vectorReader                thrpt    5  66,068 ±  7,377  ops/s
   */

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

  @Benchmark
  def serializeCaseClassToString(): Unit = {
    val json = toJSON[Product](JsonBenchmark.product)
    assert(json != null)
  }

  @Benchmark
  def vectorReader(): Unit = {
    fromJSON[Vector[Int]](JsonBenchmark.lotsOfInts)
  }

  @Benchmark
  def listReader(): Unit = {
    fromJSON[List[Int]](JsonBenchmark.lotsOfInts)
  }

  @Benchmark
  def seqReader(): Unit = {
    fromJSON[Seq[Int]](JsonBenchmark.lotsOfInts)
  }

}

case class Reference(typeId: String, id: UUID)

object Reference {
  implicit val json: JSON[Reference] = jsonProduct(apply _)
}

case class Price(id: String, value: BaseMoney)

object Price {
  implicit val json: JSON[Price] = jsonProduct(apply _)
}

case class ProductVariant(id: Long, prices: Vector[Price], attributes: Map[String, String])

object ProductVariant {
  implicit val json: JSON[ProductVariant] = jsonProduct(apply _)
}

case class Product(
  id: UUID,
  version: Long,
  productType: Reference,
  variants: Vector[ProductVariant])

object Product {
  implicit val json: JSON[Product] = jsonProduct(apply _)
}

object JsonBenchmark {

  val lotsOfInts = Range(1, 100000).mkString("[", ",", "]")

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

  val product = getFromJSON[Product](json)
}
