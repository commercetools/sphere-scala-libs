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
FromJsonBenchmark.listReader                  thrpt   10   64,787 ± 1,081  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   51,613 ± 0,338  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   69,606 ± 1,261  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   75,112 ± 1,022  ops/s

*** scala 2.13 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromJsonBenchmark.listReader                  thrpt   10   72,133 ± 2,275  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   52,469 ± 0,428  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   70,277 ± 1,062  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   72,930 ± 1,381  ops/s
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

