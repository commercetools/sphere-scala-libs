package io.sphere.json

import org.json4s.JsonAST._
import org.json4s.jackson.compactJson
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.StringWriter
import scala.util.Random

/** `JsonSink` replaces `compactJson` on the write path, so everything it emits has to be
  * byte-identical to what json4s+Jackson emitted. This is the only guard against a silent
  * wire-format change.
  */
class JsonSinkSpec extends AnyWordSpec with Matchers {

  private def rendered(j: JValue): String = {
    val s = JsonSink.buffer()
    s.jValue(j)
    s.result()
  }

  private def mustMatchJackson(j: JValue): Unit =
    rendered(j) mustEqual compactJson(j)

  "JsonSink.jValue" should {
    "match compactJson on scalars" in {
      List(
        JString(""),
        JString("plain"),
        JLong(0L),
        JLong(Long.MaxValue),
        JLong(Long.MinValue),
        JInt(BigInt("123456789012345678901234567890")),
        JInt(BigInt("-123456789012345678901234567890")),
        JDouble(0.0),
        JDouble(-0.0),
        JDouble(1.5),
        JDouble(1e300),
        JDouble(1e-300),
        JDouble(Double.MinPositiveValue),
        JDecimal(BigDecimal("1.000")),
        JDecimal(BigDecimal("-0.00000000000000000001")),
        JBool.True,
        JBool.False,
        JNull
      ).foreach(mustMatchJackson)
    }

    "quote non-finite doubles the way Jackson does" in {
      List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach(d =>
        mustMatchJackson(JDouble(d)))
    }

    "escape every char below 0x300 the same way Jackson does" in {
      (0 until 0x300).foreach { i =>
        val s = "a" + i.toChar + "b"
        withClue(f"char 0x$i%04X: ") {
          rendered(JString(s)) mustEqual compactJson(JString(s))
        }
      }
    }

    "escape random strings the same way Jackson does" in {
      val rnd = new Random(20260925L)
      (1 to 2000).foreach { _ =>
        val s = new String(Array.fill(rnd.nextInt(40))(rnd.nextInt(0x600).toChar))
        withClue(s"[${s.map(c => f"\\u${c.toInt}%04x").mkString}]: ") {
          rendered(JString(s)) mustEqual compactJson(JString(s))
        }
      }
    }

    "match compactJson on nested structures, including JNothing holes" in {
      List(
        JObject(Nil),
        JArray(Nil),
        JObject("a" -> JNothing),
        JObject("a" -> JNothing, "b" -> JLong(1L)),
        JObject("a" -> JLong(1L), "b" -> JNothing),
        JObject("a" -> JNothing, "b" -> JNothing),
        JArray(List(JNothing)),
        JArray(List(JLong(1L), JNothing, JLong(2L))),
        JObject(
          "he\"re" -> JArray(List(JObject("x" -> JNull), JNothing, JString("\t\n"))),
          "nested" -> JObject("deep" -> JObject("deeper" -> JDouble(2.25))))
      ).foreach(mustMatchJackson)
    }
  }

  "JsonSink" should {
    "grow past its initial buffer" in {
      val long = "x" * 100000
      rendered(JString(long)) mustEqual compactJson(JString(long))
    }

    "produce the same bytes when streaming to a Writer as when buffering" in {
      val j: JValue =
        JObject("a" -> JArray(List.tabulate(5000)(i => JString("value-" + i))), "b" -> JLong(7L))
      val w = new StringWriter()
      val s = JsonSink(w) // 8 KB buffer, so this flushes many times
      s.jValue(j)
      s.flush()
      w.toString mustEqual compactJson(j)
    }

    "write longs the way Jackson does" in {
      val rnd = new Random(20260925L)
      (List(0L, 1L, -1L, 9L, 10L, -10L, 99L, 100L, Long.MaxValue, Long.MinValue) ++
        List.fill(2000)(rnd.nextLong())).foreach { v =>
        val s = JsonSink.buffer()
        s.long(v)
        s.result() mustEqual v.toString
      }
    }

    "write ISO dates identically to the joda printer" in {
      val dt =
        new org.joda.time.DateTime(2026, 9, 25, 13, 45, 7, 42, org.joda.time.DateTimeZone.UTC)
      val s = JsonSink.buffer()
      s.isoUtc(dt)
      s.result() mustEqual compactJson(JString(ToJSON.printIsoUtc(dt)))
    }

    "quote and escape field prefixes" in {
      JsonSink.fieldPrefix("plain") mustEqual "\"plain\":"
      JsonSink.fieldPrefix("a\"b") mustEqual "\"a\\\"b\":"
    }
  }

  "toJSON" should {
    "keep emitting {} for a value that writes nothing" in {
      toJSON[Option[String]](None) mustEqual "{}"
      toJSON[Unit](()) mustEqual "{}"
    }
  }

  "writeJSON" should {
    "agree with toJSON" in {
      val v = Map("a" -> List(1, 2, 3), "b" -> Nil)
      val w = new StringWriter()
      writeJSON(v, w)
      w.toString mustEqual toJSON(v)
    }
  }
}
