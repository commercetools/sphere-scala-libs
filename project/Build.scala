import sbt._
import Keys._

import language._
import sbtrelease.ReleasePlugin.autoImport._
import bintray.BintrayPlugin._

object SphereLibsBuild extends Build {

  lazy val publishSettings = bintraySettings ++ Seq(
    releaseCrossBuild := true,
    UpdateVersionInFiles("sphere-util", file("README.md")),
    UpdateVersionInFiles("sphere-json", file("README.md"), file("json/README.md")),
    UpdateVersionInFiles("sphere-mongo", file("README.md"), file("mongo/README.md"))
  )

  lazy val standardSettings = Defaults.defaultSettings ++ publishSettings ++ Seq(
    organization := "io.sphere",
    scalaVersion := "2.12.2",
    crossScalaVersions := Seq("2.11.11", "2.12.2"),
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    logBuffered := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    javacOptions ++= Seq("-deprecation", "-Xlint:unchecked", "-source", "1.7", "-target", "1.7"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.3" % Test,
      "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
      "ch.qos.logback" % "logback-classic" % "1.2.3" % Test
    )
  )

  lazy val libs = Project(
    id = "sphere-libs",
    base = file("."),
    aggregate = Seq(util, json, mongo),
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
    settings = standardSettings ++ Fmpp.settings ++ Seq(
      homepage := Some(url("https://github.com/sphereio/sphere-scala-libs/blob/master/json/README.md")))//++ Seq(scalacOptions ++= Seq("-Ymacro-debug-lite"))
  ).dependsOn(util)

  lazy val mongo = Project(
    id = "sphere-mongo",
    base = file("./mongo"),
    settings = standardSettings ++ Fmpp.settings
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
