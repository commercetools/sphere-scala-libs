## to be released

* update scala to 2.11.8

## 0.6.2 (2016-01-07)

* update "com.fasterxml.jackson.core" % "jackson-databind" to "2.7.2"
* update "org.scalaz" %% "scalaz-core" to "7.1.7"
* update "joda-time" % "joda-time" to "2.9.2"
* can parse a `JLong`

## 0.6.0 (2015-12-14)

* `null` is now treated the same way as `undefined` during the case-class serialization
* Dropped scala 2.10 support

## 0.5.31 (2015-12-11)

* Added `deriveSingletonJSON` macros which provides a nice alternative to `Enumeration` where you can represent 
  enum-values as a sealed case object hierarchy (you can find an example in the [DeriveSingletonJSONSpec](https://github.com/sphereio/sphere-scala-libs/blob/master/json/src/test/scala/DeriveSingletonJSONSpec.scala)).
