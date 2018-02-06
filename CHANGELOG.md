## 0.9.4-SNAPSHOT

* [add default mongo formats for `List` and `Set`](https://github.com/sphereio/sphere-scala-libs/pull/33)
* update `jackson-databind` to `2.9.4`

## 0.9.3 (2018-01-02)

* update `json4s-jackson` to [`3.5.3`](https://github.com/json4s/json4s/compare/v3.5.2...v3.5.3)
* update `jackson-databind` to `2.9.3`
* update `joda-convert` to [`1.9.2`](http://www.joda.org/joda-convert/changes-report.html#a1.9.2)
* update `cats` to [`1.0.1`](https://github.com/typelevel/cats/releases/tag/v1.0.1)

## 0.9.2 (2017-12-14)

* `MongoFormat` uses the field default value if the field is absent in the mongo object.

## 0.9.1 (2017-12-12)

* add support for scala enums for MongoDB

## 0.9.0 (2017-09-05)

* update Cats dependency to 1.0.0-MF
* added scalac option "-Ypartial-unification"

## 0.8.2 (2017-06-15)

Please note that v0.8.0 and v0.8.1 represent the same version. They are side-effect of [failed](https://github.com/sbt/sbt-bintray/issues/104) attempt to update the `sbt-bintray` plugin.

* Introduced `sphere-mongo` which provides a set of type classes and macros to help with defining models for MongoDB (it is similar to what `sphere-json` provides for JSON models). You can use it like this:
  ```scala
  resolvers += Resolver.bintrayRepo("commercetools", "maven")

  libraryDependencies += "io.sphere" %% "sphere-mongo" % "0.8.2"
  ``` 

## 0.7.0 (2017-05-11)

* use Cats instead of Scalaz.

## 0.6.13 (2017-05-10)

* update dependencies
* optimize performance of `FromJSON[List[A]]` and `FromJSON[Vector[A]]`

## 0.6.12 (2017-04-04)

* update dependencies
* add `ToJSON.apply`

## 0.6.11 (2017-03-29)

* add ToJSONProduct.forProductN to create `ToJSON` without any magic

## 0.6.10 (2017-02-28)

* [jackson-databind 2.8.7](https://github.com/FasterXML/jackson-databind/blob/master/release-notes/VERSION)
* add Money.withCentAmount to calculate a new money with a new cent amount in the same currency

## 0.6.9 (2017-02-10)

* [jackson-databind 2.8.6](https://github.com/FasterXML/jackson-databind/blob/master/release-notes/VERSION)
* [joda-time 2.9.7](http://www.joda.org/joda-time/changes-report.html#a2.9.7)
* cross compile to scala 2.11 and scala 2.12

## 0.6.8 (2016-12-02)

* [joda-time 2.9.6](http://www.joda.org/joda-time/upgradeto290.html)
* [jackson-databind 2.8.5](https://github.com/FasterXML/jackson-databind/blob/master/release-notes/CREDITS)

## 0.6.7 (2016-11-10)

* [update scalaz to 7.1.11](https://github.com/sphereio/sphere-scala-libs/commit/35906dc805b468e5a2230226cda4a0dda44ae09d)
* [update json4s to 3.5.0](https://github.com/sphereio/sphere-scala-libs/commit/3cc6389dc252694d8b44f8818f62097ade30f4b4)
* [update joda-time to 2.9.5](https://github.com/sphereio/sphere-scala-libs/commit/2eb3b1596a218f7b83bc8ca1c174dd5af1d8d0e6)


## 0.6.6 (2016-11-01)

* [Add TypeSelectorContainer to typeswitch return type](https://github.com/sphereio/sphere-scala-libs/pull/20)
* [performance micro-optimization](https://github.com/sphereio/sphere-scala-libs/commit/8d02e8ac6ae26b6b20a026817755112fa2f38ac6)
* [update json to 3.4.2](https://github.com/sphereio/sphere-scala-libs/commit/29b295c293bba3ba5fbee62efe31b1d0c567c8d5)
* [update scala-logging to 3.5.0](https://github.com/sphereio/sphere-scala-libs/commit/264f5cb689ed00a6ca2c23b0ae1f4be6a991aa9b)

## 0.6.5 (2016-06-27)

* [more info in case of error with Money](https://github.com/sphereio/sphere-scala-libs/commit/11198ec9286631f1df70e53543487dfed170205d)

## 0.6.4 (2016-05-31)

* update "org.scalaz" %% "scalaz-core" to "7.1.8"
* update "com.fasterxml.jackson.core" % "jackson-databind" to 2.7.4
* update "joda-time" % "joda-time" to 2.9.4

## 0.6.3 (2016-04-04)

* update scala to 2.11.8
* update "com.fasterxml.jackson.core" % "jackson-databind" to 2.7.3
* update "joda-time" % "joda-time" to 2.9.3
* `JNothing` is now treated the same way as `JNull` during the case-class serialization

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
