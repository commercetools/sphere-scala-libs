package io.sphere.util

import java.util.Locale

/** Extractor for Locales, e.g. for use in pattern-matching request paths. */
object LangTag {
  class LocaleOpt(val locale: Locale) extends AnyVal {
    // if toLanguageTag returns "und", it means the language tag is undefined
    def isEmpty: Boolean = "und" == locale.toLanguageTag
    def get: Locale = locale
  }

  def unapply(s: String): LocaleOpt = new LocaleOpt(Locale.forLanguageTag(s))

  def invalidLangTagMessage(invalidLangTag: String) = s"Invalid language tag: '$invalidLangTag'"
}
