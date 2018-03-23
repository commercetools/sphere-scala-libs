val catsVersion = "1.1.0"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.5.3",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.4",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
