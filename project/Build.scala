import sbt._
import Keys._

object SphereLibsBuild extends Build {

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "io.sphere",
    scalaVersion := "2.10.4",
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
      Tests.Argument(TestFrameworks.ScalaTest, "-u", "%s" format (target / "test-reports")))
    },
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
      "ch.qos.logback" % "logback-classic" % "1.0.6" % "test"
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
    settings = standardSettings ++ Fmpp.settings //++ Seq(scalacOptions ++= Seq("-Ymacro-debug-lite"))
  )
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Code generation via FMPP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

object Fmpp {
  private lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  private lazy val fmppOptions = SettingKey[Seq[String]]("fmpp-options")
  private lazy val fmppConfig = config("fmpp") hide

  lazy val settings = fmppConfig(Compile) ++ Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.14" % fmppConfig.name,
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
