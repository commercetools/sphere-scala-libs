resolvers += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.4.0")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.5")
addSbtPlugin("io.spray" % "sbt-boilerplate" % "0.5.9")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.25")
