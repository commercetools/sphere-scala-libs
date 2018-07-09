import pl.project13.scala.sbt.JmhPlugin


lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  UpdateVersionInFiles("sphere-util", file("README.md")),
  UpdateVersionInFiles("sphere-json", file("README.md"), file("json/README.md")),
  UpdateVersionInFiles("sphere-mongo", file("README.md"), file("mongo/README.md"))
) ++ BintrayPlugin.bintraySettings

lazy val standardSettings = Defaults.coreDefaultSettings ++ publishSettings ++ Seq(
  organization := "io.sphere",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
  logBuffered := false,
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-Ypartial-unification" // SI-2712
  ),
  javacOptions ++= Seq("-deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7"),
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % Test,
    "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
    "ch.qos.logback" % "logback-classic" % "1.2.3" % Test
  ),
  shellPrompt in ThisBuild := { state ⇒
    scala.Console.CYAN + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val `sphere-libs` = project.in(file("."))
  .settings(standardSettings: _*)
  .settings(
    publishArtifact := false,
    publish := {})
  .disablePlugins(BintrayPlugin)
  .aggregate(`sphere-util`, `sphere-json`, `sphere-mongo`)

lazy val `sphere-util` = project.in(file("./util"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(url("https://github.com/sphereio/sphere-scala-libs/README.md")))

lazy val `sphere-json` = project.in(file("./json"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .settings(

    mappings in (Compile, packageSrc) := {
      val base  = (sourceManaged in Compile).value
      val files = (managedSources in Compile).value

      // pick managed sources only
      val collected =
        files.map {  f  => {
            val path = f.relativeTo(base).get.getPath
            val pathWithoutFmpp = path.replaceAll("\\.fmpp(\\.[^\\.]+)$", "$1")

            (f, pathWithoutFmpp)
          }
        }

      collected
    }
  )
  .settings(homepage := Some(url("https://github.com/sphereio/sphere-scala-libs/blob/master/json/README.md")))
  .dependsOn(`sphere-util`)

lazy val `sphere-mongo` = project.in(file("./mongo"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-util`)

// benchmarks

lazy val benchmarks = project
  .settings(standardSettings: _*)
  .settings(
    publishArtifact := false,
    publish := {})
  .enablePlugins(JmhPlugin)
  .disablePlugins(BintrayPlugin)
  .dependsOn(`sphere-util`, `sphere-json`)

