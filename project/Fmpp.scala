import sbt._
import Keys._
import xsbti.FileConverter

import java.io.File

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Code generation via FMPP
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// inspiration: https://github.com/sbt/sbt-fmpp/blob/master/src/main/scala/FmppPlugin.scala

object Fmpp {
  lazy val fmpp = TaskKey[Seq[File]]("fmpp")
  private lazy val fmppOptions = SettingKey[Seq[String]]("fmppOptions")
  private lazy val FmppConfig = config("fmpp").hide

  lazy val settings = fmppConfigSettings(Compile) ++ Seq(
    libraryDependencies += "net.sourceforge.fmpp" % "fmpp" % "0.9.16" % FmppConfig.name,
    ivyConfigurations += FmppConfig,
    FmppConfig / fullClasspath := Def.uncached {
      implicit val conv: FileConverter = fileConverter.value
      update.value
        .select(configurationFilter(FmppConfig.name))
        .map(f => Attributed.blank(conv.toVirtualFile(f.toPath)))
    }
  )

  private def fmppConfigSettings(c: Configuration): Seq[Setting[?]] = inConfig(c)(
    Seq(
      fmpp := Def.uncached(fmppTask.value),
      sources := managedSources.value
    ))

  private val fmppTask = Def.task {
    val cp = (FmppConfig / fullClasspath).value
    implicit val conv: FileConverter = fileConverter.value
    val r = (fmpp / runner).value
    val srcRoot = baseDirectory.value / (Defaults.prefix(configuration.value.name) + "src")
    val output = sourceManaged.value
    val s = streams.value
    val cache = s.cacheDirectory

    val cached = FileFunction.cached(cache / "fmpp", FilesInfo.lastModified, FilesInfo.exists) {
      (in: Set[File]) =>
        IO.delete(output)
        val arguments =
          "-U" +: "all" +: "-S" +: srcRoot.getAbsolutePath +: "-O" +: output.getAbsolutePath +:
            "-M" +: "ignore(test/**,it/**),execute(**/*.fmpp.scala),copy(**/*)" +: Seq.empty
        r.run("fmpp.tools.CommandLine", cp.files, arguments, s.log)
          .failed
          .foreach(t => sys.error(t.getMessage))
        (output ** "*.scala").get().toSet ++ (output ** "*.java").get().toSet
    }
    cached((srcRoot ** "*.*").get().toSet).toSeq
  }
}
