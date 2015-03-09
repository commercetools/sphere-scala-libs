import sbt._
import Keys._
import language._
import sbtrelease.ReleasePlugin._
import bintray.Plugin._

object SphereLibsBuild extends Build {

  lazy val publishSettings = releaseSettings ++ bintraySettings ++ Seq(
    ReleaseKeys.crossBuild := true,
    UpdateVersionInFiles("sphere-util", file("README.md"), file("json/README.md")),
    UpdateVersionInFiles("sphere-json", file("README.md"), file("json/README.md"))
  )

  lazy val standardSettings = Defaults.defaultSettings ++ publishSettings ++ Seq(
    organization := "io.sphere",
    licenses := Seq("Apache License, ASL Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := "2.10.5",
    crossScalaVersions := Seq("2.10.5", "2.11.6"),
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    logBuffered := false,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    javacOptions ++= Seq("-deprecation", "-Xlint:unchecked"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "test",
      "ch.qos.logback" % "logback-classic" % "1.0.6" % "test"
    ),
    libraryDependencies += "org.scala-lang" % "scalap" % scalaVersion.value
  )

  lazy val libs = Project(
    id = "sphere-libs",
    base = file("."),
    aggregate = Seq(util, json),
    settings = standardSettings
  ).settings (
    publishArtifact := false
  )

  lazy val util = Project(
    id = "sphere-util",
    base = file("./util"),
    settings = standardSettings
  )

  lazy val json = Project(
    id = "sphere-json",
    base = file("./json"),
    //dependencies = Seq(util),
    settings = standardSettings ++ Fmpp.settings //++ Seq(scalacOptions ++= Seq("-Ymacro-debug-lite"))
  ).dependsOn(util)
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
    fullClasspath in fmppConfig <<= update map { _ select configurationFilter(fmppConfig.name) map Attributed.blank }
  )

  private def fmppConfig(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    sourceGenerators in Compile <+= fmpp,
    fmpp <<= fmppTask,
    scalaSource <<= (baseDirectory, configuration) { (base, c) => base / (Defaults.prefix(c.name) + "src") },
    sources <<= managedSources
  ))

  private val fmppTask =
    (fullClasspath in fmppConfig, runner in fmpp, unmanagedSources, scalaSource, sourceManaged, fmppOptions, streams) map {
      (cp, r, sources, srcRoot, output, args, s) => {
        IO.delete(output)
        val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +:
          "-M" +: "ignore(test/**,it/**),execute(**/*.fmpp.scala),copy(**/*)" +: Seq.empty
        toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
        (output ** "*.scala").get ++ (output ** "*.java").get
      }
    }
}
