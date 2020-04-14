package json

import io.sphere.json._
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class FromJsonBenchmark {

  /* on local mac
  jmh:run

*** scala 2.12 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromJsonBenchmark.listReader                  thrpt   10   65,822 ± 4,552  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   13,583 ± 0,433  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   68,325 ± 1,412  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   70,977 ± 1,907  ops/s

*** scala 2.13 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromJsonBenchmark.listReader                  thrpt   10   68,492 ± 1,393  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   14,076 ± 0,148  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   63,854 ± 1,515  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   67,712 ± 1,236  ops/s
   */

  @Benchmark
  def parseFromStringToCaseClass(): Unit = {
    val product = getFromJSON[Product](JsonBenchmark.json)
    assert(product.version == 2)
  }

  @Benchmark
  def vectorReader(): Unit = {
    fromJSON[Vector[Int]](JsonBenchmark.lotsOfIntsAsJson)
  }

  @Benchmark
  def listReader(): Unit = {
    fromJSON[List[Int]](JsonBenchmark.lotsOfIntsAsJson)
  }

  @Benchmark
  def seqReader(): Unit = {
    fromJSON[Seq[Int]](JsonBenchmark.lotsOfIntsAsJson)
  }

}

