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

Benchmark                                      Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                    thrpt   10  70,065 ± 0,967  ops/s
ToJsonBenchmark.seqWriter                     thrpt   10  63,512 ± 3,889  ops/s
ToJsonBenchmark.serializeCaseClassToString    thrpt   10  37,925 ± 2,252  ops/s
ToJsonBenchmark.vectorWriter                  thrpt   10  63,762 ± 1,470  ops/s
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
