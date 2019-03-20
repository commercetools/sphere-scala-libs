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
FromJsonBenchmark.listReader                  thrpt   10  66,286 ± 1,025  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10  12,974 ± 0,333  ops/s
FromJsonBenchmark.seqReader                   thrpt   10  66,626 ± 1,235  ops/s
FromJsonBenchmark.vectorReader                thrpt   10  67,702 ± 2,501  ops/s

*** scala 2.13 ***
Benchmark                                      Mode  Cnt   Score   Error  Units
FromJsonBenchmark.listReader                  thrpt   10  58,223 ± 1,198  ops/s
FromJsonBenchmark.parseFromStringToCaseClass  thrpt   10  12,929 ± 0,094  ops/s
FromJsonBenchmark.seqReader                   thrpt   10  59,705 ± 1,311  ops/s
FromJsonBenchmark.vectorReader                thrpt   10  62,816 ± 0,750  ops/s
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

