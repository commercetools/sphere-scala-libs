val catsVersion = "1.0.0-MF"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging"  %% "scala-logging"  % "3.7.2",
  "joda-time"                   %  "joda-time"      % "2.9.9",
  "org.joda"                    %  "joda-convert"   % "1.8.2",
  "org.typelevel"               %% "cats-macros"    % catsVersion,
  "org.typelevel"               %% "cats-core"      % catsVersion,
  "org.json4s"                  %% "json4s-scalap"  % "3.5.3"
)
