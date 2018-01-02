val catsVersion = "1.0.0-MF"

libraryDependencies ++= Seq(
  "org.json4s"                  %% "json4s-jackson"   % "3.5.3",
  "com.fasterxml.jackson.core"  %  "jackson-databind" % "2.9.3",
  "org.typelevel"               %% "cats-macros"      % catsVersion,
  "org.typelevel"               %% "cats-core"        % catsVersion
)
