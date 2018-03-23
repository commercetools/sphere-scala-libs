package json

import io.sphere.json.fromJSON
import org.json4s.StringInput
import org.json4s.jackson._
import org.openjdk.jmh.annotations.Benchmark

class JsonBenchmark {

  /* on local mac
  jmh:run -i 10 -wi 10 -f1 -t1

[info] Benchmark                       Mode  Cnt   Score    Error  Units
[info] JsonBenchmark.listReader       thrpt   10  41,167 ± 10,925  ops/s
[info] JsonBenchmark.parseFromString  thrpt   10  58,816 ±  4,951  ops/s
[info] JsonBenchmark.vectorReader     thrpt   10  49,893 ±  2,880  ops/s
   */

  @Benchmark
  def parseFromString(): Unit = {
    parseJson(StringInput(JsonBenchmark.json))
  }

  @Benchmark
  def vectorReader(): Unit = {
    fromJSON[Vector[Int]](JsonBenchmark.lotsOfInts)
  }

  @Benchmark
  def listReader(): Unit = {
    fromJSON[List[Int]](JsonBenchmark.lotsOfInts)
  }

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
}
