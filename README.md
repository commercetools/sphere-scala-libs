sphere-scala-libs
=================

Just some Scala libraries that started out as internal projects as part of the sphere.io platform and
have been made public in the hope that they might be useful to more people.

[![Build Status](https://travis-ci.org/commercetools/sphere-scala-libs.png)](https://travis-ci.org/commercetools/sphere-scala-libs)

## Including in the build

The library is published on the [Bintray](https://bintray.com/commercetools/maven):

    resolvers += Resolver.bintrayRepo("commercetools", "maven")
    
    libraryDependencies += "io.sphere" %% "sphere-util" % "0.5.1"
    libraryDependencies += "io.sphere" %% "sphere-json" % "0.5.1"
    
    