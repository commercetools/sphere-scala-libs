package json

import java.util.UUID

import io.sphere.json._
import io.sphere.json.generic._
import io.sphere.util.BaseMoney
import org.json4s.StringInput
import org.json4s.jackson._
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ToJsonBenchmark {

  /* on local mac
  jmh:run

*** scala 2.12 ***
Benchmark                                    Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                    thrpt   10   70,604 ± 1,277  ops/s
ToJsonBenchmark.seqWriter                     thrpt   10   28,650 ± 0,311  ops/s
ToJsonBenchmark.serializeCaseClassToString    thrpt   10   51,404 ± 0,748  ops/s
ToJsonBenchmark.vectorWriter                  thrpt   10   61,722 ± 1,770  ops/s

*** scala 2.13 ***
Benchmark                                    Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                    thrpt   10   73,688 ±  1,381  ops/s
ToJsonBenchmark.seqWriter                     thrpt   10   70,049 ±  1,697  ops/s
ToJsonBenchmark.serializeCaseClassToString    thrpt   10   29,107 ± 0,417  ops/s
ToJsonBenchmark.vectorWriter                  thrpt   10   70,300 ±  1,833  ops/s
   */

  @Benchmark
  def serializeCaseClassToString(): Unit = {
    val json = toJSON[Product](JsonBenchmark.product)
    assert(json != null)
  }

  @Benchmark
  def vectorWriter(): Unit =
    toJSON[Vector[Int]](JsonBenchmark.lotsOfIntsVector)

  @Benchmark
  def listWriter(): Unit =
    toJSON[List[Int]](JsonBenchmark.lotsOfIntsList)

  @Benchmark
  def seqWriter(): Unit =
    toJSON[Seq[Int]](JsonBenchmark.lotsOfIntsSeq)

}
