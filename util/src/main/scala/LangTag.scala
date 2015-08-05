package io.sphere.util

import java.util.Locale

/** Extractor for Locales, e.g. for use in pattern-matching request paths. */
object LangTag {
  def unapply(s: String): Option[Locale] = {
    val locale = Locale.forLanguageTag(s)
    // if toLanguageTag returns "und", it means the language tag is undefined
    if (locale.toLanguageTag().equals("und")) None else Some(locale)
  }

  def invalidLangTagMessage(invalidLangTag: String) = s"Invalid language tag: '$invalidLangTag'"
}
