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
FromJsonBenchmark.listReader                  thrpt   10   69,925 ± 0,776  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   19,010 ± 0,186  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   72,595 ± 0,737  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   71,278 ± 0,896  ops/s

*** scala 2.13 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromJsonBenchmark.listReader                  thrpt   10   70,661 ±  1,623  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   19,288 ±  0,118  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   72,897 ±  1,317  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   72,016 ±  0,552  ops/s
   */

  @Benchmark
  def parseFromStringToCaseClass(): Unit = {
    val product = getFromJSON[Product](JsonBenchmark.json)
    assert(product.version == 2)
  }

  @Benchmark
  def vectorReader(): Unit =
    fromJSON[Vector[Int]](JsonBenchmark.lotsOfIntsAsJson)

  @Benchmark
  def listReader(): Unit =
    fromJSON[List[Int]](JsonBenchmark.lotsOfIntsAsJson)

  @Benchmark
  def seqReader(): Unit =
    fromJSON[Seq[Int]](JsonBenchmark.lotsOfIntsAsJson)

}
