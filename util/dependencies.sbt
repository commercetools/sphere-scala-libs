val catsVersion = "0.9.0"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "joda-time" % "joda-time" % "2.9.9",
  "org.joda" % "joda-convert" % "1.8.1",
  "org.typelevel" %% "cats-macros" % catsVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.json4s" %% "json4s-scalap" % "3.5.2"
)
