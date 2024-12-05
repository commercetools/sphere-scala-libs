import pl.project13.scala.sbt.JmhPlugin

lazy val scala2_12 = "2.12.19"
lazy val scala2_13 = "2.13.14"
lazy val scala3 = "3.5.2"

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq(scala2_12, scala2_13, scala3)
ThisBuild / scalaVersion := scala2_13
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
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq("-noindent")
    case _ => Seq.empty
  }),
  javacOptions ++= Seq("-deprecation", "-Xlint:unchecked"),
  // targets Java 8 bytecode (scalac & javac)
  ThisBuild / scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.13")) Seq("-release", "8")
    else Nil
  },
  ThisBuild / javacOptions ++= Seq("-source", "8", "-target", "8"),
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % Test,
    "org.scalacheck" %% "scalacheck" % "1.18.0" % Test,
    "ch.qos.logback" % "logback-classic" % "1.5.6" % Test
  ),
  ThisBuild / shellPrompt := { state ⇒
    scala.Console.CYAN + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val `sphere-libs` = project
  .in(file("."))
  .settings(standardSettings: _*)
  .settings(
    crossScalaVersions := Nil,
    publishArtifact := false,
    publish := {}
  )
  .aggregate(
    // Scala 3 modules
    `sphere-util-3`,
    `sphere-json-3`,

    // Scala 2 modules
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

// Scala 3 modules

lazy val `sphere-util-3` = project
  .in(file("./util-3"))
  .settings(scalaVersion := scala3)
  .settings(standardSettings: _*)
  .settings(homepage := Some(url("https://github.com/commercetools/sphere-scala-libs/README.md")))

lazy val `sphere-json-3` = project
  .in(file("./json/json-3"))
  .settings(scalaVersion := scala3)
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-util-3`)

// Scala 2 modules

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

lazy val `sphere-json-derivation-scala-3` = project
  .settings(scalaVersion := scala3)
  .in(file("./json/json-derivation-scala-3"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-json-core`)

lazy val `sphere-json` = project
  .in(file("./json"))
  .settings(standardSettings: _*)
  .settings(homepage := Some(
    url("https://github.com/commercetools/sphere-scala-libs/blob/master/json/README.md")))
  .dependsOn(`sphere-json-core`, `sphere-json-derivation`)

lazy val `sphere-mongo-core` = project
  .in(file("./mongo/mongo-core"))
  .settings(crossScalaVersions := Seq(scala3, scala2_13))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

lazy val `sphere-mongo-derivation` = project
  .in(file("./mongo/mongo-derivation"))
  .settings(standardSettings: _*)
  .settings(Fmpp.settings: _*)
  .dependsOn(`sphere-mongo-core`)

lazy val `sphere-mongo-3` = project
  .settings(scalaVersion := scala3)
  .in(file("./mongo/mongo-3"))
  .settings(standardSettings: _*)
  .dependsOn(`sphere-util`)

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

lazy val benchmarks = project
  .settings(standardSettings: _*)
  .settings(publishArtifact := false, publish := {})
  .enablePlugins(JmhPlugin)
  .dependsOn(`sphere-util`, `sphere-json`, `sphere-mongo`)
