val catsVersion = "1.6.0"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging"  %% "scala-logging"  % "3.9.0",
  "joda-time"                   %  "joda-time"      % "2.10.1",
  "org.joda"                    %  "joda-convert"   % "2.2.0",
  "org.typelevel"               %% "cats-macros"    % catsVersion,
  "org.typelevel"               %% "cats-core"      % catsVersion,
  "org.json4s"                  %% "json4s-scalap"  % "3.6.5"
)
