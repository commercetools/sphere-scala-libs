package json

import org.json4s.StringInput
import org.json4s.jackson._
import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ParseJsonBenchmark {

  /* on local mac
  jmh:run

Benchmark                                      Mode  Cnt   Score   Error  Units
ParseJsonBenchmark.parseFromStringToJValue    thrpt   10  85,322 ± 1,073  ops/s
   */

  @Benchmark
  def parseFromStringToJValue(): Unit = {
    val jvalue = parseJson(StringInput(JsonBenchmark.json))
    assert(jvalue != null)
  }
}

