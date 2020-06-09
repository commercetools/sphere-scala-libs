package json

import io.sphere.mongo.format.fromMongo
import io.sphere.mongo.format.DefaultMongoFormats._
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class FromMongoBenchmark {

  /* on local mac
jmh:run

*** scala 2.13 ***
Benchmark                                  Mode  Cnt     Score    Error  Units
FromMongoBenchmark.mongoValueToCaseClass  thrpt   10    25,306 ±  0,832  ops/s
FromMongoBenchmark.mongoValueToList       thrpt   10   521,449 ± 17,672  ops/s
FromMongoBenchmark.mongoValueToMap        thrpt   10    51,554 ±  0,648  ops/s
FromMongoBenchmark.mongoValueToVector     thrpt   10  1334,286 ± 21,065  ops/s
 */

  @Benchmark
  def mongoValueToCaseClass(): Unit = {
    val product = fromMongo[Product](JsonBenchmark.productMongoValue)
    assert(product.version == 2)
  }

  @Benchmark
  def mongoValueToVector(): Unit = {
    val vector = fromMongo[Vector[Int]](JsonBenchmark.lotsOfIntsMongoValue)
    assert(JsonBenchmark.lotsOfIntsVector.size == vector.size)
  }

  @Benchmark
  def mongoValueToList(): Unit = {
    val list = fromMongo[List[Int]](JsonBenchmark.lotsOfIntsMongoValue)
    assert(JsonBenchmark.lotsOfIntsVector.size == list.size)
  }

  @Benchmark
  def mongoValueToMap(): Unit = {
    val map = fromMongo[Map[String, String]](JsonBenchmark.bigMapMongoValue)
    assert(JsonBenchmark.bigMap.size == map.size)
  }

}
