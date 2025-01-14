import pl.project13.scala.sbt.JmhPlugin

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq("2.12.20", "2.13.15")
ThisBuild / scalaVersion := crossScalaVersions.value.last
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowJavaVersions := List(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting"))
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
  ThisBuild / scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.12")) Seq.empty
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
  .settings(publishArtifact := false, publish := {})
  .aggregate(
    `sphere-util`,
    `sphere-json`,
    `sphere-json-core`,
    `sphere-json-derivation`,
    `sphere-mongo`,
    `sphere-mongo-core`,
    `sphere-mongo-derivation`,
    `sphere-mongo-derivation-magnolia`,
    `benchmarks`
  )

lazy val `sphere-util` = project
  .in(file("./util"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(url("https://github.com/commercetools/sphere-scala-libs/README.md")))

lazy val `sphere-json-core` = project
  .in(file("./json/json-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-json-derivation` = project
  .in(file("./json/json-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-json-core`)

lazy val `sphere-json` = project
  .in(file("./json"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/json/README.md")))
  .dependsOn(`sphere-json-core`, `sphere-json-derivation`)

lazy val `sphere-mongo-core` = project
  .in(file("./mongo/mongo-core"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-mongo-derivation` = project
  .in(file("./mongo/mongo-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-mongo-core`)

lazy val `sphere-mongo-derivation-magnolia` = project
  .in(file("./mongo/mongo-derivation-magnolia"))
  .settings(standardSettings: _*)
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
