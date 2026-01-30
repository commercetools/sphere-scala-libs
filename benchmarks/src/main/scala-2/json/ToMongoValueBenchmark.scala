package json

import io.sphere.mongo.format._
import io.sphere.mongo.format.DefaultMongoFormats._
import org.openjdk.jmh.annotations.{
  Benchmark,
  BenchmarkMode,
  Fork,
  Measurement,
  Mode,
  Scope,
  State,
  Warmup
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ToMongoValueBenchmark {

  /* on local mac
jmh:run

[info] Benchmark                                     Mode  Cnt    Score    Error  Units
[info] ToMongoValueBenchmark.caseClassToMongoValue  thrpt   10   76,492 ±  0,968  ops/s
[info] ToMongoValueBenchmark.listToMongoValue       thrpt   10  484,802 ± 16,722  ops/s
[info] ToMongoValueBenchmark.mapToMongoValueTo      thrpt   10   30,316 ±  3,938  ops/s
[info] ToMongoValueBenchmark.vectorToMongoValue     thrpt   10  671,930 ± 17,021  ops/s
   */

  @Benchmark
  def caseClassToMongoValue(): Unit = {
    val mongoValue = toMongo[Product](JsonBenchmark.product)
    assert(mongoValue != null)
  }

  @Benchmark
  def vectorToMongoValue(): Unit = {
    val mongoValue = toMongo[Vector[Int]](JsonBenchmark.lotsOfIntsVector)
    assert(mongoValue != null)
  }

  @Benchmark
  def listToMongoValue(): Unit = {
    val mongoValue = toMongo[List[Int]](JsonBenchmark.lotsOfIntsList)
    assert(mongoValue != null)
  }

  @Benchmark
  def mapToMongoValueTo(): Unit = {
    val mongoValue = toMongo[Map[String, String]](JsonBenchmark.bigMap)
    assert(mongoValue != null)
  }

}
