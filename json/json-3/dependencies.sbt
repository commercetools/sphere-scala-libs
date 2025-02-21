libraryDependencies ++= Seq(
  ("org.json4s" %% "json4s-jackson" % "4.0.7").cross(CrossVersion.binary),
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.17.2",
  ("org.typelevel" %% "cats-core" % "2.13.0").cross(CrossVersion.binary)
)
