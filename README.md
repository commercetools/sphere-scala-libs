sphere-scala-libs
=================

Just some Scala libraries that started out as internal projects as part of the [commercetools platform](http://dev.commercetools.com/) (that was originally named sphere.io) and have been made public in the hope that they might be useful to more people.

[![Build Status](https://travis-ci.org/sphereio/sphere-scala-libs.svg)](https://travis-ci.org/sphereio/sphere-scala-libs)

## Download

sphere-json: [ ![Download](https://api.bintray.com/packages/commercetools/maven/sphere-json/images/download.svg) ](https://bintray.com/commercetools/maven/sphere-json/_latestVersion)

sphere-util: [ ![Download](https://api.bintray.com/packages/commercetools/maven/sphere-util/images/download.svg) ](https://bintray.com/commercetools/maven/sphere-util/_latestVersion)

sphere-mongo: [ ![Download](https://api.bintray.com/packages/commercetools/maven/sphere-mongo/images/download.svg) ](https://bintray.com/commercetools/maven/sphere-mongo/_latestVersion)

## Documentation

[sphere-json](json/README.md)

## Including in the build

The library is published to the [Bintray](https://bintray.com/commercetools/maven):

    resolvers += Resolver.bintrayRepo("commercetools", "maven")
    
    libraryDependencies += "io.sphere" %% "sphere-util" % "0.9.11"
    libraryDependencies += "io.sphere" %% "sphere-json" % "0.9.11"
    libraryDependencies += "io.sphere" %% "sphere-mongo" % "0.9.11"

## License

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
