val catsVersion = "1.2.0"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.5.3",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.6",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
