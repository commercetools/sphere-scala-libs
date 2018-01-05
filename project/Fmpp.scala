import sbt._
import Keys._

import java.io.File

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Code generation via FMPP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// inspiration: https://github.com/sbt/sbt-fmpp/blob/master/src/main/scala/FmppPlugin.scala

object Fmpp {
  private lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  private lazy val fmppOptions = SettingKey[Seq[String]]("fmpp-options")
  private lazy val fmppConfig = config("fmpp") hide

  lazy val settings = fmppConfig(Compile) ++ Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.15" % fmppConfig.name,
    ivyConfigurations += fmppConfig,
    fmppOptions := "--ignore-temporary-files" :: Nil,
    fullClasspath in fmppConfig := update.value select configurationFilter(fmppConfig.name) map Attributed.blank
  )

  private def fmppConfig(c: Configuration): Seq[Setting[_]] = inConfig(c)(Seq(
    sourceGenerators in Compile += fmpp.taskValue,
    fmpp := fmppTask.value,
    sources := managedSources.value
  ))

  private val fmppTask = Def.task {
    val cp = (fullClasspath in fmppConfig).value
    val r = (runner in fmpp).value
    val srcRoot = baseDirectory.value / (Defaults.prefix(configuration.value.name) + "src")
    val output = sourceManaged.value
    val s = streams.value
    val cache = s.cacheDirectory

    val cached = FileFunction.cached(cache / "fmpp", FilesInfo.lastModified, FilesInfo.exists) {
      (in: Set[File]) => {
        IO.delete(output)
        val arguments = "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +:
          "-M" +: "ignore(test/**,it/**),execute(**/*.fmpp.scala),copy(**/*)" +: Seq.empty
        toError(r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log))
        (output ** "*.scala").get.toSet ++ (output ** "*.java").get.toSet
      }
    }
    cached((srcRoot ** "*.*").get.toSet).toSeq
  }
}
