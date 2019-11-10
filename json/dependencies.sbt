val catsVersion = "2.0.0"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.6.7",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.10.1",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
