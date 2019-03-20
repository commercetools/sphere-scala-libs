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
ToJsonBenchmark.listWriter                  thrpt   10  68,770 ± 1,157  ops/s
ToJsonBenchmark.seqWriter                   thrpt   10  65,792 ± 1,191  ops/s
ToJsonBenchmark.serializeCaseClassToString  thrpt   10  39,142 ± 0,574  ops/s
ToJsonBenchmark.vectorWriter                thrpt   10  64,359 ± 2,162  ops/s

*** scala 2.13 ***
Benchmark                                    Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                  thrpt   10  71,097 ± 1,392  ops/s
ToJsonBenchmark.seqWriter                   thrpt   10  56,812 ± 1,715  ops/s
ToJsonBenchmark.serializeCaseClassToString  thrpt   10  53,280 ± 0,993  ops/s
ToJsonBenchmark.vectorWriter                thrpt   10  54,754 ± 1,563  ops/s
   */

  @Benchmark
  def serializeCaseClassToString(): Unit = {
    val json = toJSON[Product](JsonBenchmark.product)
    assert(json != null)
  }

  @Benchmark
  def vectorWriter(): Unit = {
    toJSON[Vector[Int]](JsonBenchmark.lotsOfIntsVector)
  }

  @Benchmark
  def listWriter(): Unit = {
    toJSON[List[Int]](JsonBenchmark.lotsOfIntsList)
  }

  @Benchmark
  def seqWriter(): Unit = {
    toJSON[Seq[Int]](JsonBenchmark.lotsOfIntsSeq)
  }

}
