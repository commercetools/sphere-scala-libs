val catsVersion = "1.6.1"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.6.6",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.9",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
