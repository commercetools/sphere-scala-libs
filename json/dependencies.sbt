val catsVersion = "0.9.0"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.5.2",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.8.1",
  "org.typelevel" %% "cats-macros" % catsVersion,
  "org.typelevel" %% "cats-core" % catsVersion
)
