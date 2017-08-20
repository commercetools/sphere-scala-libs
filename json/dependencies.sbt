val catsVersion = "1.0.0-MF"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.5.2",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.8.9",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
