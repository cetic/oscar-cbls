addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.17")

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.7")

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.9.0")

// Only used to build, on demand, the standalone "fat jar" of the multi-JVM examples.
// It does not take part in the artifacts published to the Maven repository.
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.2.0")
