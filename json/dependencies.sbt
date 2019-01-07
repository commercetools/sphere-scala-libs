val catsVersion = "1.5.0"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.6.3",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.7",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
