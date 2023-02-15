package json

import io.sphere.json.{FromJSON, JValidation}
import org.joda.time.DateTime
import org.json4s.JsonAST.JString
import org.openjdk.jmh.annotations._

import scala.annotation.nowarn

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
@nowarn("msg=unused value of type")
class DateTimeFromJSONBenchmark {

  @Param(
    Array(
      "2025-12-14T12:50:25.070Z",
      "2022-09-05T00:18:33.994Z",
    ))
  var rawValue: String = _
  private var json: JString = _

  @Setup
  final def setupJson(): Unit =
    json = JString(rawValue)

  import FromJSON.dateTimeReader
  @Benchmark
  final def deserializeDateTime(): JValidation[DateTime] =
    dateTimeReader.read(json)
}
