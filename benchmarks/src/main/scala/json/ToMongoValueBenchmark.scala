package json

import io.sphere.mongo.format._
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ToMongoValueBenchmark {

  @Benchmark
  def caseClassToMongoValue(): Unit = {
    val mongoValue = toMongo[Product](JsonBenchmark.product)
    assert(mongoValue != null)
  }

}
