val catsVersion = "2.0.0"

libraryDependencies ++= Seq(
  "com.propensive" %% "magnolia" % "0.12.0",
  "org.json4s"                  %% "json4s-jackson"   % "3.6.7",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.9.3",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
