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

Benchmark                                                    Mode  Cnt         Score    Error   Units
ToJsonBenchmark.listWriter                                  thrpt   20      1517,382 ±  4,195   ops/s
ToJsonBenchmark.listWriter:gc.alloc.rate.norm               thrpt   20   4775220,595 ±  0,013    B/op
ToJsonBenchmark.listWriterViaJValue                         thrpt   20       975,423 ±  6,620   ops/s
ToJsonBenchmark.listWriterViaJValue:gc.alloc.rate.norm      thrpt   20   7027103,140 ±  0,054    B/op
ToJsonBenchmark.seqWriter                                   thrpt   20      1420,348 ±  4,287   ops/s
ToJsonBenchmark.seqWriter:gc.alloc.rate.norm                thrpt   20   4775260,909 ±  0,024    B/op
ToJsonBenchmark.seqWriterViaJValue                          thrpt   20       800,426 ± 42,928   ops/s
ToJsonBenchmark.seqWriterViaJValue:gc.alloc.rate.norm       thrpt   20   8625121,856 ±  4,138    B/op
ToJsonBenchmark.serializeCaseClassToString                  thrpt   20       437,194 ±  8,370   ops/s
ToJsonBenchmark.serializeCaseClassToString:gc.alloc...norm  thrpt   20  19989824,040 ±  0,363    B/op
ToJsonBenchmark.serializeCaseClassToWriter                  thrpt   20       205,240 ±  2,231   ops/s
ToJsonBenchmark.serializeCaseClassToWriter:gc.alloc...norm  thrpt   20  13450441,854 ±  0,394    B/op
ToJsonBenchmark.serializeCaseClassToWriterViaSink           thrpt   20       518,427 ±  6,610   ops/s
ToJsonBenchmark...ToWriterViaSink:gc.alloc.rate.norm        thrpt   20    250325,471 ±  0,221    B/op
ToJsonBenchmark.vectorWriter                                thrpt   20      1283,295 ±  7,132   ops/s
ToJsonBenchmark.vectorWriter:gc.alloc.rate.norm             thrpt   20   4775301,432 ±  0,033    B/op
ToJsonBenchmark.vectorWriterViaJValue                       thrpt   20       802,343 ± 25,237   ops/s
ToJsonBenchmark.vectorWriterViaJValue:gc.alloc.rate.norm    thrpt   20   7027217,848 ±  3,374    B/op
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
