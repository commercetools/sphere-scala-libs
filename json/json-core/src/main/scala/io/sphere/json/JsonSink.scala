package io.sphere.json

import com.fasterxml.jackson.core.io.CharTypes
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import org.json4s.JsonAST._

import java.io.Writer

/** Append-only JSON writer that `ToJSON.writeTo` threads through the value being serialized,
  * instead of building a `JValue` tree and handing it to Jackson afterwards.
  *
  * Output is byte-identical to `org.json4s.jackson.compactJson`; `JsonSinkSpec` pins that.
  *
  * Chars accumulate in an internal buffer. With a `Writer` target the buffer is flushed whenever it
  * fills, so memory is bounded; without one (`JsonSink.buffer()`) it grows and `result()` returns
  * the whole document.
  *
  * Not thread-safe, and not reusable across documents.
  */
final class JsonSink private (out: Writer, initialCapacity: Int) {
  private var buf = new Array[Char](initialCapacity)
  private var pos = 0

  private def ensure(n: Int): Unit =
    if (pos + n > buf.length) {
      if (out ne null) flush()
      if (pos + n > buf.length) {
        val next = new Array[Char](math.max(buf.length * 2, pos + n))
        System.arraycopy(buf, 0, next, 0, pos)
        buf = next
      }
    }

  /** A single structural character: `{`, `}`, `[`, `]`, `,` or `:`. */
  def ch(c: Char): Unit = {
    ensure(1)
    buf(pos) = c
    pos += 1
  }

  /** Appends `s` verbatim. The caller guarantees it is already valid JSON — use this for
    * pre-quoted, pre-escaped literals such as field-name prefixes.
    */
  def raw(s: String): Unit = {
    val n = s.length
    ensure(n)
    s.getChars(0, n, buf, pos)
    pos += n
  }

  /** Appends `s` as a quoted, escaped JSON string. */
  def string(s: String): Unit = {
    val n = s.length
    if (JsonSink.isClean(s)) {
      ensure(n + 2)
      buf(pos) = '"'
      s.getChars(0, n, buf, pos + 1)
      buf(pos + n + 1) = '"'
      pos += n + 2
    } else {
      ensure(n * 6 + 2)
      buf(pos) = '"'
      pos += 1
      var i = 0
      while (i < n) {
        pos = JsonSink.escapeOne(s.charAt(i), buf, pos)
        i += 1
      }
      buf(pos) = '"'
      pos += 1
    }
  }

  def long(v: Long): Unit =
    // -Long.MinValue is still Long.MinValue, so it cannot go through the negate branch.
    if (v == Long.MinValue) raw("-9223372036854775808")
    else if (v < 0) { ch('-'); long(-v) }
    else {
      ensure(19)
      var start = pos
      var x = v
      while ({
        buf(pos) = ('0' + (x % 10).toInt).toChar
        pos += 1
        x /= 10
        x != 0
      }) ()
      var end = pos - 1
      while (start < end) {
        val t = buf(start)
        buf(start) = buf(end)
        buf(end) = t
        start += 1
        end -= 1
      }
    }

  /** Matches Jackson's `writeNumber(double)`, which quotes non-finite values because
    * `QUOTE_NON_NUMERIC_NUMBERS` is on by default.
    */
  def double(v: Double): Unit = {
    val s = java.lang.Double.toString(v)
    if (v.isNaN || v.isInfinite) string(s) else raw(s)
  }

  def boolean(v: Boolean): Unit = raw(if (v) "true" else "false")

  def nullValue(): Unit = raw("null")

  /** `"yyyy-MM-ddTHH:mm:ss.SSSZ"`, quotes included, written in place. Equivalent to
    * `string(ISODateTimeFormat.dateTime.print(dt.withZone(UTC)))` — see `ToJSON.printIsoUtc`.
    */
  def isoUtc(dt: DateTime): Unit = {
    val u = if (dt.getZone eq DateTimeZone.UTC) dt else dt.withZone(DateTimeZone.UTC)
    val y = u.getYear
    if (y < 0 || y > 9999) string(ISODateTimeFormat.dateTime.print(u))
    else {
      ensure(26)
      val b = buf
      var i = pos
      b(i) = '"'
      put2(b, i + 1, y / 100)
      put2(b, i + 3, y % 100)
      b(i + 5) = '-'
      put2(b, i + 6, u.getMonthOfYear)
      b(i + 8) = '-'
      put2(b, i + 9, u.getDayOfMonth)
      b(i + 11) = 'T'
      put2(b, i + 12, u.getHourOfDay)
      b(i + 14) = ':'
      put2(b, i + 15, u.getMinuteOfHour)
      b(i + 17) = ':'
      put2(b, i + 18, u.getSecondOfMinute)
      b(i + 20) = '.'
      val ms = u.getMillisOfSecond
      b(i + 21) = ('0' + ms / 100).toChar
      put2(b, i + 22, ms % 100)
      b(i + 24) = 'Z'
      b(i + 25) = '"'
      pos = i + 26
    }
  }

  private def put2(b: Array[Char], i: Int, v: Int): Unit = {
    b(i) = ('0' + v / 10).toChar
    b(i + 1) = ('0' + v % 10).toChar
  }

  /** Fallback path for `ToJSON` instances that have not overridden `writeTo`. Renders exactly what
    * json4s' `JValueSerializer` would.
    */
  def jValue(j: JValue): Unit = j match {
    case JString(v) => string(v)
    case JLong(v) => long(v)
    case JInt(v) => raw(v.bigInteger.toString)
    case JDouble(v) => double(v)
    case JDecimal(v) => raw(v.bigDecimal.toString)
    case JBool(v) => boolean(v)
    case JObject(fields) =>
      ch('{')
      var wrote = false
      fields.foreach { f =>
        if (f._2 ne JNothing) {
          if (wrote) ch(',') else wrote = true
          string(f._1)
          ch(':')
          jValue(f._2)
        }
      }
      ch('}')
    case JArray(elements) =>
      ch('[')
      var wrote = false
      elements.foreach { e =>
        if (e ne JNothing) {
          if (wrote) ch(',') else wrote = true
          jValue(e)
        }
      }
      ch(']')
    case JSet(elements) =>
      ch('[')
      var wrote = false
      elements.foreach { e =>
        if (e ne JNothing) {
          if (wrote) ch(',') else wrote = true
          jValue(e)
        }
      }
      ch(']')
    case JNull => nullValue()
    case JNothing => ()
  }

  /** Pushes everything buffered so far to the `Writer`. No-op for a buffering sink. */
  def flush(): Unit =
    if (out ne null) {
      out.write(buf, 0, pos)
      pos = 0
    }

  /** The document written so far. Only meaningful for `JsonSink.buffer()`. */
  def result(): String = new String(buf, 0, pos)
}

object JsonSink {

  /** A sink that streams to `out`, flushing whenever its buffer fills. */
  def apply(out: Writer): JsonSink = new JsonSink(out, 8192)

  /** A sink that accumulates the whole document, readable via `result()`. */
  def buffer(): JsonSink = new JsonSink(null, 4096)

  /** `"name":`, quoted and escaped once so the write path can `raw` it. */
  def fieldPrefix(name: String): String = {
    val s = buffer()
    s.string(name)
    s.ch(':')
    s.result()
  }

  /** Jackson's own table: index = char, 0 = emit as-is, >0 = the short escape letter, -1 = generic
    * six-char unicode escape. Using it keeps the "which characters need escaping, and how" decision
    * identical to the Jackson path we are replacing.
    */
  private final val Esc: Array[Int] = CharTypes.get7BitOutputEscapes()
  private final val Hex = "0123456789ABCDEF" // Jackson emits uppercase hex digits

  private def isClean(s: String): Boolean = {
    val n = s.length
    var i = 0
    while (i < n) {
      val c = s.charAt(i)
      if (c < 128 && Esc(c) != 0) return false
      i += 1
    }
    true
  }

  /** Writes the escaped form of `c` at `p`, returns the new position. */
  private def escapeOne(c: Char, b: Array[Char], p: Int): Int =
    if (c >= 128) { b(p) = c; p + 1 }
    else {
      val e = Esc(c)
      if (e == 0) { b(p) = c; p + 1 }
      else if (e > 0) { b(p) = '\\'; b(p + 1) = e.toChar; p + 2 }
      else {
        b(p) = '\\'
        b(p + 1) = 'u'
        b(p + 2) = '0'
        b(p + 3) = '0'
        b(p + 4) = Hex.charAt((c >> 4) & 0xf)
        b(p + 5) = Hex.charAt(c & 0xf)
        p + 6
      }
    }
}
