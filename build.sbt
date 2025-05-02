import pl.project13.scala.sbt.JmhPlugin

lazy val scala213 = "2.13.16"
lazy val scala3 = "3.3.5"

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / scalaVersion := scala213
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowJavaVersions := List(JavaSpec.temurin("21"))
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting"))
)
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

// workaround for CI because `sbt ++3.3.4 test` used by sbt-github-actions
// still tries to compile the Scala 2 only projects leading to weird issues
// note that `sbt +test` is working fine to run cross-compiled tests locally
ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    commands = List(
      "sphere-util/test",
      "sphere-json/test",
      "sphere-json-core/test",
      "sphere-json-derivation/test",
      "sphere-mongo/test",
      "sphere-mongo-core/test",
      "sphere-mongo-derivation/test",
      "benchmarks/test"
    ),
    name = Some("Build Scala 2 project"),
    cond = Some(s"matrix.scala != '$scala3'")
  ),
  WorkflowStep.Sbt(
    commands = List(
      "sphere-util/test",
      "sphere-mongo-core/test",
      "sphere-json-core/test"
    ),
    name = Some("Build Scala 3 project"),
    cond = Some(s"matrix.scala == '$scala3'")
  )
)

// Release

inThisBuild(
  List(
    organization := "com.commercetools",
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    homepage := Some(url("https://github.com/commercetools/sphere-scala-libs")),
    developers := List(
      Developer(
        id = "commercetools",
        name = "commercetools",
        email = "ondemand@commercetools.com",
        url = url("https://commercetools.com"))),
    githubWorkflowTargetTags ++= Seq("v*"),
    githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),
    githubWorkflowPublish := Seq(WorkflowStep.Sbt(
      List("ci-release"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    ))
  ))

lazy val standardSettings = Defaults.coreDefaultSettings ++ Seq(
  logBuffered := false,
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  javacOptions ++= Seq("-deprecation", "-Xlint:unchecked"),
  // targets Java 8 bytecode (scalac & javac)
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("3")) Seq("-noindent")
    else Seq("-target", "8")
  },
  ThisBuild / javacOptions ++= Seq("-source", "8", "-target", "8"),
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % Test,
    "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
    "ch.qos.logback" % "logback-classic" % "1.5.16" % Test
  ),
  ThisBuild / shellPrompt := { state ⇒
    scala.Console.CYAN + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val `sphere-libs` = project
  .in(file("."))
  .settings(standardSettings: _*)
  .settings(publishArtifact := false, publish := {}, crossScalaVersions := Seq())
  .aggregate(
    `sphere-util`,
    `sphere-json`,
    `sphere-json-core`,
    `sphere-json-derivation`,
    `sphere-mongo`,
    `sphere-mongo-core`,
    `sphere-mongo-derivation`,
    `benchmarks`
  )

// Scala 2 & 3 modules

lazy val `sphere-util` = project
  .in(file("./util"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(url("https://github.com/commercetools/sphere-scala-libs/README.md")))

lazy val `sphere-json-core` = project
  .in(file("./json/json-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-mongo-core` = project
  .in(file("./mongo/mongo-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

// Scala 2 modules

lazy val `sphere-json-derivation` = project
  .in(file("./json/json-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .settings(
    inConfig(Compile)(
      sourceGenerators ++= (if (scalaVersion.value.startsWith("2")) Seq(Fmpp.fmpp.taskValue)
                            else Seq())))
  .dependsOn(`sphere-json-core`)

lazy val `sphere-json` = project
  .in(file("./json"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/json/README.md")))
  .dependsOn(`sphere-json-core`, `sphere-json-derivation`)

lazy val `sphere-mongo-derivation` = project
  .in(file("./mongo/mongo-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .settings(
    inConfig(Compile)(
      sourceGenerators ++= (if (scalaVersion.value.startsWith("2")) Seq(Fmpp.fmpp.taskValue)
      else Seq())))
  .dependsOn(`sphere-mongo-core`)

lazy val `sphere-mongo` = project
  .in(file("./mongo"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/mongo/README.md")))
  .dependsOn(`sphere-mongo-core`, `sphere-mongo-derivation`)

// benchmarks

lazy val benchmarks = project
  .settings(standardSettings: _*)
  .settings(publishArtifact := false, publish := {})
  .enablePlugins(JmhPlugin)
  .dependsOn(`sphere-util`, `sphere-json`, `sphere-mongo`)
