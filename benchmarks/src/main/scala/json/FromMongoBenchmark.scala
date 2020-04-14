package json

import io.sphere.mongo.format.fromMongo
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class FromMongoBenchmark {

  @Benchmark
  def mongoValueToCaseClass(): Unit = {
    val product = fromMongo[Product](JsonBenchmark.productMongoValue)
    assert(product.version == 2)
  }

}
