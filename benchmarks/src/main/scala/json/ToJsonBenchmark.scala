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
ToJsonBenchmark.listWriter                    thrpt   10   71,408 ± 2,115  ops/s
ToJsonBenchmark.seqWriter                     thrpt   10   65,315 ± 1,585  ops/s
ToJsonBenchmark.serializeCaseClassToString    thrpt   10   47,129 ± 1,004  ops/s
ToJsonBenchmark.vectorWriter                  thrpt   10   61,818 ± 1,314  ops/s

*** scala 2.13 ***
Benchmark                                    Mode  Cnt   Score   Error  Units
ToJsonBenchmark.listWriter                    thrpt   10   76,453 ± 1,897  ops/s
ToJsonBenchmark.seqWriter                     thrpt   10   71,084 ± 1,736  ops/s
ToJsonBenchmark.serializeCaseClassToString    thrpt   10   56,433 ± 0,991  ops/s
ToJsonBenchmark.vectorWriter                  thrpt   10   67,755 ± 1,906  ops/s
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
