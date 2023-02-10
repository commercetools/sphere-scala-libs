package json

import io.sphere.json.generic.fromJsonEnum
import io.sphere.json.{FromJSON, JValidation}
import org.json4s.JsonAST.JString
import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit
import scala.annotation.nowarn

object FewCasesEnum extends Enumeration {
  val First, Middle, End = Value
}

object ManyCasesEnum extends Enumeration {
  val First, A, B, C, D, E, F, G, H, I, J, K, L, Middle, N, O, P, Q, R, S, T, U, V, W, X, Y, Z,
      End = Value
}

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
@nowarn("msg=unused value of type")
class EnumFromJSONBenchmark {
  private val fromJSONForManyCases: FromJSON[ManyCasesEnum.Value] = fromJsonEnum(ManyCasesEnum)
  private val fromJSONForFewCases: FromJSON[FewCasesEnum.Value] = fromJsonEnum(FewCasesEnum)

  @Param(
    Array(
      "First",
      "Middle",
      "End"
    ))
  var rawValue: String = _
  private var json: JString = _

  @Setup
  final def setupJson(): Unit =
    json = JString(rawValue)

  @Benchmark
  final def enumWithManyCasesFromJson(): JValidation[ManyCasesEnum.Value] =
    fromJSONForManyCases.read(json)

  @Benchmark
  final def enumWithFewCasesFromJson(): JValidation[FewCasesEnum.Value] =
    fromJSONForFewCases.read(json)

}
