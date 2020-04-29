package json

import io.sphere.mongo.format._
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ToMongoValueBenchmark {

  /* on local mac
jmh:run

*** scala 2.12 ***
Benchmark                                     Mode  Cnt   Score   Error  Units
ToMongoValueBenchmark.caseClassToMongoValue   thrpt   10  79,065 ± 1,075  ops/s

*** scala 2.13 ***
Benchmark                                     Mode  Cnt   Score   Error  Units
ToMongoValueBenchmark.caseClassToMongoValue   thrpt   10  80,242 ± 1,033  ops/s
*/


  @Benchmark
  def caseClassToMongoValue(): Unit = {
    val mongoValue = toMongo[Product](JsonBenchmark.product)
    assert(mongoValue != null)
  }

}
