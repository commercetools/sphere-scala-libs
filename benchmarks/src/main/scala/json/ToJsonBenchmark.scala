package json

import io.sphere.json._
import org.json4s.jackson.JsonMethods
import org.openjdk.jmh.annotations._

import java.io.{OutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(value = 1)
class ToJsonBenchmark {

  /* on local mac (M-series, JDK 21), scala 2.13.18
  sbt "++2.13.18" "benchmarks/Jmh/run -prof gc -f 2 ToJsonBenchmark"

  `*ViaJValue` / `serializeCaseClassToWriter` are the pre-sink path, kept as in-run controls.
  Full write-up in docs/json-serialization-perf.md.

  serializeCaseClassToWriter       200,5 ± 2,6   ops/s   13 450 443 B/op   <- control
  serializeCaseClassToWriterViaSink  513,6 ± 2,8 ops/s      250 326 B/op
  serializeCaseClassToString       437,2 ± 8,4   ops/s   19 989 824 B/op
  typeSwitchWriterViaJValue        122,2 ± 22,2  ops/s   33 730 755 B/op   <- control
  typeSwitchWriter                 297,3 ± 10,1  ops/s   21 785 285 B/op
  listWriterViaJValue              975,4 ± 6,6   ops/s    7 027 103 B/op   <- control
  listWriter                      1517,4 ± 4,2   ops/s    4 775 221 B/op
  seqWriterViaJValue               800,4 ± 42,9  ops/s    8 625 122 B/op   <- control
  seqWriter                       1420,3 ± 4,3   ops/s    4 775 261 B/op
  vectorWriterViaJValue            802,3 ± 25,2  ops/s    7 027 218 B/op   <- control
  vectorWriter                    1283,3 ± 7,1   ops/s    4 775 301 B/op
   */

  @Benchmark
  def serializeCaseClassToString(): String =
    toJSON[Product](JsonBenchmark.product)

  /** Mirrors the production consumer, which streams into an OutputStreamWriter rather than
    * materializing a String:
    * {{{
    * case class ResponseJValue(jValue: JValue) extends ResponseWriter {
    *   def write(writer: OutputStreamWriter): Unit =
    *     JsonMethods.mapper.writeValue(writer, jValue)
    * }
    * }}}
    */
  @Benchmark
  def serializeCaseClassToWriter(): Unit = {
    val writer = new OutputStreamWriter(OutputStream.nullOutputStream(), StandardCharsets.UTF_8)
    JsonMethods.mapper.writeValue(writer, toJValue[Product](JsonBenchmark.product))
  }

  /** The new sink path: no `JValue` tree, no Jackson, straight into the Writer. */
  @Benchmark
  def serializeCaseClassToWriterViaSink(): Unit = {
    val writer = new OutputStreamWriter(OutputStream.nullOutputStream(), StandardCharsets.UTF_8)
    writeJSON[Product](JsonBenchmark.product, writer)
  }

  /** Fails the run rather than reporting throughput for output that drifted. */
  @Setup
  def checkByteEquality(): Unit = {
    val mine = toJSON[Product](JsonBenchmark.product)
    val theirs = JsonMethods.compact(toJValue[Product](JsonBenchmark.product))
    if (mine != theirs) {
      val i = mine.zip(theirs).indexWhere { case (a, b) => a != b }
      sys.error(s"""sink output differs from compactJson at $i:
                   |  sink:   ${mine.slice(i - 60, i + 60)}
                   |  json4s: ${theirs.slice(i - 60, i + 60)}""".stripMargin)
    }
    val w = new java.io.StringWriter()
    writeJSON[Product](JsonBenchmark.product, w)
    if (w.toString != theirs) sys.error("writeJSON differs from compactJson")
  }

  @Benchmark
  def vectorWriter(): String =
    toJSON[Vector[Int]](JsonBenchmark.lotsOfIntsVector)

  @Benchmark
  def listWriter(): String =
    toJSON[List[Int]](JsonBenchmark.lotsOfIntsList)

  @Benchmark
  def seqWriter(): String =
    toJSON[Seq[Int]](JsonBenchmark.lotsOfIntsSeq)

  /** A `jsonTypeSwitch` payload. Until `writeFieldsTo` existed these fell back to building a
    * `JValue`, so they got none of the sink's gain.
    */
  @Benchmark
  def typeSwitchWriter(): String =
    toJSON[Vector[UpdateAction]](JsonBenchmark.lotsOfActions)

  @Benchmark
  def typeSwitchWriterViaJValue(): String =
    JsonMethods.compact(toJValue[Vector[UpdateAction]](JsonBenchmark.lotsOfActions))

  // Controls: the pre-sink path (build a JValue, hand it to Jackson), measured in the same run so
  // the comparison is not against numbers from another JVM.
  @Benchmark
  def seqWriterViaJValue(): String =
    JsonMethods.compact(toJValue[Seq[Int]](JsonBenchmark.lotsOfIntsSeq))

  @Benchmark
  def vectorWriterViaJValue(): String =
    JsonMethods.compact(toJValue[Vector[Int]](JsonBenchmark.lotsOfIntsVector))

  @Benchmark
  def listWriterViaJValue(): String =
    JsonMethods.compact(toJValue[List[Int]](JsonBenchmark.lotsOfIntsList))

}
