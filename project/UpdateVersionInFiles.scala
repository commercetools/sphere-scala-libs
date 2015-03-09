import java.io.File
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStep
import sbtrelease.ReleaseStateTransformations._
import sbt._
import sbt.Keys._

// source: https://github.com/EECOLOR/sbt-release-custom-steps/blob/master/src%2Fmain%2Fscala%2Forg%2Fqirx%2Fsbtrelease%2FUpdateVersionInFiles.scala

object UpdateVersionInFiles {

  def apply(namePattern: String, files: File*) =
    releaseProcess := withUpdatedFiles(namePattern, releaseProcess.value, files)

  def withUpdatedFiles(namePattern: String, steps: Seq[ReleaseStep], files: Seq[File]) =
    insert(
      step = updateVersionInFiles(namePattern, files),
      before = commitReleaseVersion,
      in = steps)

  def insert(step: ReleaseStep, before: ReleaseStep, in: Seq[ReleaseStep]) = {

    val (beforeStep, rest) = in.span(_ != before)
    (beforeStep :+ step) ++ rest
  }

  def updateVersionInFiles(namePattern: String, files: Seq[File]): ReleaseStep = { s: State =>
    val settings = Project.extract(s)

    val pattern = getPattern(namePattern, settings)
    val version = settings.get(releaseVersion)(settings.get(Keys.version))
    val replacement = "$1" + version + "$2"

    def updateFile(file: File) = {
      s.log.info(s"Update version occurrences in '${file.getName}' with $replacement")
      val contents = IO.read(file)
      val newContents = contents.replaceAll(pattern, replacement)
      if (contents != newContents)
        s.log.info(s"Updated version occurrences in '${file.getName}'")
      IO.write(file, newContents)
      vcs(settings).add(file.getAbsolutePath) !! s.log
    }

    files.foreach(updateFile)

    s
  }

  def getPattern(namePattern: String, settings: Extracted) = {

    val organization = settings.get(Keys.organization)
    val name = namePattern
    val % = "\"\\s+%+\\s+\"" // " %% " or "   % "
    val > = "(\""
    val < = ")"
    val versionPattern = "[\\w\\.-]+"

    // "org.qirx" %% "sbt-webjar" % "version"
    > + organization + % + name + % + < + versionPattern + > + <
  }

  private def vcs(settings:Extracted) =
    settings.get(versionControlSystem)
      .getOrElse(sys.error("Aborting release. Working directory is not a repository of a recognized VCS."))
}