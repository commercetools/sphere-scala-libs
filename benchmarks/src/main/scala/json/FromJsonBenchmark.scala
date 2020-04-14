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
FromJsonBenchmark.listReader                  thrpt   10   65,643 ± 1,436  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   13,785 ± 0,206  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   68,488 ± 1,299  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   70,445 ± 0,850  ops/s

*** scala 2.13 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromJsonBenchmark.listReader                  thrpt   10   63,963 ± 1,789  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10   14,135 ± 0,137  ops/s
FromJsonBenchmark.seqReader                   thrpt   10   69,365 ± 1,275  ops/s
FromJsonBenchmark.vectorReader                thrpt   10   70,401 ± 1,839  ops/s
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

