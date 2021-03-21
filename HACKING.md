# Hacking

To hack on this project, you need [sbt](http://www.scala-sbt.org/)


# Release a new version

The release is done via `https://github.com/olafurpg/sbt-ci-release`, see the related workflow in `.github/workflows/release.yaml`.

To trigger a new release simply create a tag and push it:

```
git tag -a v0.1.0 -m "v0.1.0"
git push origin v0.1.0
```
