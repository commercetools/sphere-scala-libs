val catsVersion = "1.6.0"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.6.5",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.8",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
