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
  sbt "++2.13.18" "benchmarks/Jmh/run -prof gc ToJsonBenchmark"

Benchmark                                                       Mode  Cnt         Score     Error   Units
ToJsonBenchmark.listWriter                                     thrpt   10      1121,275 ±  25,115   ops/s
ToJsonBenchmark.listWriter:gc.alloc.rate.norm                  thrpt   10   7027102,240 ±   0,153    B/op
ToJsonBenchmark.seqWriter                                      thrpt   10      1033,565 ±   7,566   ops/s
ToJsonBenchmark.seqWriter:gc.alloc.rate.norm                   thrpt   10   7027134,764 ±   0,052    B/op
ToJsonBenchmark.serializeCaseClassToString                     thrpt   10       197,962 ±   4,458   ops/s
ToJsonBenchmark.serializeCaseClassToString:gc.alloc.rate.norm  thrpt   10  25462539,315 ±   0,871    B/op
ToJsonBenchmark.serializeCaseClassToWriter                     thrpt   10       211,499 ±   1,519   ops/s
ToJsonBenchmark.serializeCaseClassToWriter:gc.alloc.rate.norm  thrpt   10  13450441,015 ±   0,292    B/op
ToJsonBenchmark.vectorWriter                                   thrpt   10       941,722 ±  11,396   ops/s
ToJsonBenchmark.vectorWriter:gc.alloc.rate.norm                thrpt   10   7027199,427 ±   0,099    B/op
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

  @Benchmark
  def vectorWriter(): String =
    toJSON[Vector[Int]](JsonBenchmark.lotsOfIntsVector)

  @Benchmark
  def listWriter(): String =
    toJSON[List[Int]](JsonBenchmark.lotsOfIntsList)

  @Benchmark
  def seqWriter(): String =
    toJSON[Seq[Int]](JsonBenchmark.lotsOfIntsSeq)

}
