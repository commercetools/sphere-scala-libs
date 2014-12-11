import sbt._
import Keys._
import language._
import sbtrelease.ReleasePlugin._
import bintray.Plugin._

object SphereLibsBuild extends Build {

  lazy val publishSettings = releaseSettings ++ bintraySettings

  lazy val standardSettings = Defaults.defaultSettings ++ bintrayResolverSettings ++ Seq(
    organization := "io.sphere",
    scalaVersion := "2.10.4",
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    logBuffered := false,
    publishMavenStyle := false,
    publishTo := Some("bintray-public" at "https://api.bintray.com/maven/commercetools/maven/sphere-libs"),
    resolvers += "sphere-public" at "http://dl.bintray.com/commercetools/maven",
    credentials ++= Seq(Credentials(Path.userHome / ".bintray-credentials")),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    javacOptions ++= Seq("-deprecation", "-Xlint:unchecked"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
      "ch.qos.logback" % "logback-classic" % "1.0.6" % "test"
    ),
    libraryDependencies += "org.scala-lang" % "scalap" % scalaVersion.value
  )

  lazy val libs = Project(
    id = "sphere-libs",
    base = file("."),
    aggregate = Seq(util, json),
    settings = standardSettings
  )

  lazy val util = Project(
    id = "sphere-util",
    base = file("./util"),
    settings = standardSettings ++ publishSettings
  )

  lazy val json = Project(
    id = "sphere-json",
    base = file("./json"),
    settings = standardSettings ++ publishSettings ++ spray.boilerplate.BoilerplatePlugin.Boilerplate.settings˚
  )
}