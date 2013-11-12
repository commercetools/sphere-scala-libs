import sbt._
import Keys._

object SphereLibsBuild extends Build {

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "io.sphere",
    scalaVersion := "2.10.0",
    logBuffered := false,
    publishTo <<= (version) { version: String =>
      if(version.trim.endsWith("SNAPSHOT"))
        Some("ct-snapshots" at "http://repo.ci.cloud.commercetools.de/content/repositories/snapshots")
      else
        Some("ct-public-releases" at "http://public-repo.ci.cloud.commercetools.de/content/repositories/releases")
    },
    resolvers += "sphere-public" at "http://public-repo.ci.cloud.commercetools.de/content/repositories/releases",
    credentials ++= Seq(
      Credentials(Path.userHome / ".ivy2" / ".ct-credentials"),
      Credentials(Path.userHome / ".ivy2" / ".ct-credentials-public")),
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature"),
    javacOptions ++= Seq("-deprecation", "-Xlint:unchecked"),
    testOptions in Test <<= (target in Test) map { target => Seq(
      Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
      Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (target / "test-reports")))
    },
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
      "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
      "ch.qos.logback" % "logback-classic" % "0.9.27" % "test"
    ),
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scalap" % _)
  )

  lazy val libs = Project(
    id = "sphere-libs",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(util, json)
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
    settings = standardSettings ++ spray.boilerplate.BoilerplatePlugin.Boilerplate.settings //++ Seq(scalacOptions ++= Seq("-Ymacro-debug-lite"))
  )
}