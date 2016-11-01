import sbt._
import Keys._

import language._
import sbtrelease.ReleasePlugin.autoImport._
import bintray.BintrayPlugin._

object SphereLibsBuild extends Build {

  lazy val publishSettings = bintraySettings ++ Seq(
    releaseCrossBuild := true,
    UpdateVersionInFiles("sphere-util", file("README.md"), file("json/README.md")),
    UpdateVersionInFiles("sphere-json", file("README.md"), file("json/README.md"))
  )

  lazy val standardSettings = Defaults.defaultSettings ++ publishSettings ++ Seq(
    organization := "io.sphere",
    scalaVersion := "2.11.8",
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    logBuffered := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    javacOptions ++= Seq("-deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.3" % Test,
      "ch.qos.logback" % "logback-classic" % "1.1.7" % Test
    )
  )

  lazy val libs = Project(
    id = "sphere-libs",
    base = file("."),
    aggregate = Seq(util, json),
    settings = standardSettings
  ).settings (
    publishArtifact := false,
    publish := {}
  ).disablePlugins(bintray.BintrayPlugin)

  lazy val util = Project(
    id = "sphere-util",
    base = file("./util"),
    settings = standardSettings ++ Seq(
      homepage := Some(url("https://github.com/sphereio/sphere-scala-libs/README.md")))
  )

  lazy val json = Project(
    id = "sphere-json",
    base = file("./json"),
    //dependencies = Seq(util),
    settings = standardSettings ++ Fmpp.settings ++ Seq(
      homepage := Some(url("https://github.com/sphereio/sphere-scala-libs/blob/master/json/README.md")))//++ Seq(scalacOptions ++= Seq("-Ymacro-debug-lite"))
  ).dependsOn(util)

  // benchmarks

  lazy val benchmarks = {
    import pl.project13.scala.sbt.JmhPlugin
    Project(
      id = "benchmarks",
      base = file("./benchmarks"),
      dependencies = Seq(util, json),
      settings = standardSettings ++ Seq(
        publishArtifact := false,
        publish := {}
      )
    ).enablePlugins(JmhPlugin).disablePlugins(bintray.BintrayPlugin)
  }

}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Code generation via FMPP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

object Fmpp {
  private lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  private lazy val fmppOptions = SettingKey[Seq[String]]("fmpp-options")
  private lazy val fmppConfig = config("fmpp") hide

  lazy val settings = fmppConfig(Compile) ++ Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.15" % fmppConfig.name,
    ivyConfigurations += fmppConfig,
    fmppOptions := "--ignore-temporary-files" :: Nil,
    fullClasspath in fmppConfig := update.value select configurationFilter(fmppConfig.name) map Attributed.blank
  )

  private def fmppConfig(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    sourceGenerators in Compile += fmpp.taskValue,
    fmpp := fmppTask.value,
    scalaSource := baseDirectory.value / (Defaults.prefix(configuration.value.name) + "src"),
    sources := managedSources.value
  ))

  private val fmppTask = Def.task {
    val cp = (fullClasspath in fmppConfig).value
    val r = (runner in fmpp).value
    val srcRoot = scalaSource.value
    val output = sourceManaged.value
    val s = streams.value
    IO.delete(output)
    val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +:
      "-M" +: "ignore(test/**,it/**),execute(**/*.fmpp.scala),copy(**/*)" +: Seq.empty
    toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
    (output ** "*.scala").get ++ (output ** "*.java").get
  }
}
