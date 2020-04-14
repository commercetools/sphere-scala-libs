package json

import io.sphere.mongo.format.fromMongo
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class FromMongoBenchmark {

  /* on local mac
jmh:run

*** scala 2.12 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromMongoBenchmark.mongoValueToCaseClass      thrpt   10  292,099 ± 6,138  ops/s

*** scala 2.13 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromMongoBenchmark.mongoValueToCaseClass      thrpt   10  322,000 ± 7,792  ops/s
 */

  @Benchmark
  def mongoValueToCaseClass(): Unit = {
    val product = fromMongo[Product](JsonBenchmark.productMongoValue)
    assert(product.version == 2)
  }

}
