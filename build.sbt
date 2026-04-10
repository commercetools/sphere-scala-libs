import pl.project13.scala.sbt.JmhPlugin

lazy val scala212 = "2.12.21"
lazy val scala213 = "2.13.18"
lazy val scala3 = "3.3.7"

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / scalaVersion := scala3
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowJavaVersions := List(JavaSpec.temurin("21"))
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting"))
)
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

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

val scalaTestVersion = "3.2.20"
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
    if (scalaVersion.value.startsWith("2.12")) Seq.empty
    else if (scalaVersion.value.startsWith("3")) Seq("-noindent", "-Wunused:imports")
    else Seq("-target", "8", "-Wunused:imports")
  },
  ThisBuild / javacOptions ++= Seq("-source", "8", "-target", "8"),
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % Test,
    "org.scalacheck" %% "scalacheck" % "1.19.0" % Test,
    "ch.qos.logback" % "logback-classic" % "1.5.32" % Test
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
    `sphere-util-test`,
    `sphere-json`,
    `sphere-json-core`,
    `sphere-json-derivation`,
    `sphere-mongo`,
    `sphere-mongo-core`,
    `sphere-mongo-derivation`,
    `benchmarks`
  )

lazy val `sphere-util` = project
  .in(file("./util"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(url("https://github.com/commercetools/sphere-scala-libs/README.md")))
  .dependsOn(`sphere-util-test` % Test)

lazy val `sphere-util-test` = project
  .in(file("./util-test"))
  .settings(standardSettings: _*)
  .settings(libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalaTestVersion))
  .settings(publishArtifact := false, publish := {})
  .settings(crossScalaVersions := Seq(scala212, scala213, scala3))
  .settings(homepage := Some(url("https://github.com/commercetools/sphere-scala-libs/README.md")))

lazy val `sphere-json-core` = project
  .in(file("./json/json-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-mongo-core` = project
  .in(file("./mongo/mongo-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)
  .dependsOn(`sphere-util-test` % Test)

def excludeFromScalafix(file: File): Boolean =
  file.getName.endsWith(".fmpp.scala") || file.getName.endsWith("Macros.scala")

lazy val `sphere-json-derivation` = project
  .in(file("./json/json-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .settings(
    Compile / scalafix / unmanagedSources ~= (_.filterNot(excludeFromScalafix)),
    Test / scalafix / unmanagedSources ~= (_.filterNot(excludeFromScalafix))
  )
  .settings(
    inConfig(Compile)(
      sourceGenerators ++= (if (scalaVersion.value.startsWith("2")) Seq(Fmpp.fmpp.taskValue)
                            else Seq())))
  .dependsOn(`sphere-json-core`)
  .dependsOn(`sphere-util-test` % Test)

lazy val `sphere-json` = project
  .in(file("./json"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/json/README.md")))
  .dependsOn(`sphere-json-core`, `sphere-json-derivation`)

lazy val `sphere-mongo-core` = project
  .in(file("./mongo/mongo-core"))
  .settings(standardSettings: _*)
  .settings(crossScalaVersions := Seq(scala212, scala213, scala3))
  .dependsOn(`sphere-util`)
  .dependsOn(`sphere-util-test` % Test)

lazy val `sphere-mongo-derivation` = project
  .in(file("./mongo/mongo-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .settings(
    Compile / scalafix / unmanagedSources ~= (_.filterNot(excludeFromScalafix)),
    Test / scalafix / unmanagedSources ~= (_.filterNot(excludeFromScalafix))
  )
  .settings(
    inConfig(Compile)(
      sourceGenerators ++= (if (scalaVersion.value.startsWith("2")) Seq(Fmpp.fmpp.taskValue)
                            else Seq())))
  .dependsOn(`sphere-mongo-core`)
  .dependsOn(`sphere-util-test` % Test)

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
