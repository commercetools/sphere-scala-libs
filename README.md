sphere-scala-libs
=================

Just some Scala libraries that started out as internal projects as part of the sphere.io platform and
have been made public in the hope that they might be useful to more people.

[![Build Status](https://travis-ci.org/sphereio/sphere-scala-libs.png)](https://travis-ci.org/sphereio/sphere-scala-libs)

## Download

sphere-json: [ ![Download](https://api.bintray.com/packages/commercetools/maven/sphere-json/images/download.svg) ](https://bintray.com/commercetools/maven/sphere-json/_latestVersion)

sphere-util: [ ![Download](https://api.bintray.com/packages/commercetools/maven/sphere-util/images/download.svg) ](https://bintray.com/commercetools/maven/sphere-util/_latestVersion)

## Including in the build

The library is published to the [Bintray](https://bintray.com/commercetools/maven):

    resolvers += Resolver.bintrayRepo("commercetools", "maven")
    
    libraryDependencies += "io.sphere" %% "sphere-util" % "0.5.7"
    libraryDependencies += "io.sphere" %% "sphere-json" % "0.5.7"

## License

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).