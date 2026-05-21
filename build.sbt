ThisBuild / scalaVersion  := "2.13.17"
ThisBuild / organization  := "be.cetic"
ThisBuild / versionScheme := Some("early-semver")

val pekkoVersion = "1.4.0"

// Maven central metadata
ThisBuild / homepage := Some(url("https://github.com/cetic/oscar-cbls"))
ThisBuild / licenses := List("LGPL-3.0" -> url("https://www.gnu.org/licenses/lgpl-3.0.en.html"))
ThisBuild / developers := List(
  Developer(
    id    = "cetic",
    name  = "CETIC",
    email = "info@cetic.be",
    url   = url("https://www.cetic.be")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/cetic/oscar-cbls"),
    "scm:git@github.com:cetic/oscar-cbls.git"
  )
)
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"

lazy val oscarCbls = (project in file("."))
  .enablePlugins(PackPlugin)
  .settings(
    name := "oscar-cbls",
    licenses += ("LGPL-3.0", url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    // Auto map external jar when possible
    Compile / doc / autoAPIMappings := true,
    Compile / doc / scalacOptions ++= Seq("-groups"),
    Compile / scalacOptions ++= Seq(
      // Refer to https://docs.scala-lang.org/overviews/compiler-options/index.html for details
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:postfixOps", // not officially recommended, but often used in the project
      "-opt-warnings:at-inline-failed-summary",
      "-opt:l:inline",
      "-opt-inline-from:oscar.**"
    ) ++ (if (!OscarBuildParameters.enableAssertions) Seq("-Xdisable-assertions") else Seq.empty),
  )
  .settings(PackPlugin.packSettings)
  .settings(packGenerateWindowsBatFile := false)
  .settings(
    libraryDependencies ++= Seq(
      // GUI dependencies
      "com.gluonhq"        % "maps"            % "2.0.0-ea+6",
      "org.scalafx"       %% "scalafx"         % "22.0.0-R33",
      // Test dependencies
      "junit"              % "junit"           % "4.13.2"  % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.19.0",
      "org.scalatest"     %% "scalatest"       % "3.2.19",
      "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
      // Scala parallel collections
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
      // Pekko Core - Actor system
      "org.apache.pekko" %% "pekko-actor-typed" % pekkoVersion,
      // Pekko Cluster - For distributed actors
      "org.apache.pekko" %% "pekko-cluster-typed" % pekkoVersion,
      // Pekko Remote - Required for cross-JVM communication
      "org.apache.pekko" %% "pekko-remote" % pekkoVersion,
      // Pekko Serialization Jackson (keep for compatibility)
      "org.apache.pekko" %% "pekko-serialization-jackson" % pekkoVersion,
      // Kryo Serialization - High performance binary serialization
      "io.altoo" %% "pekko-kryo-serialization" % "1.3.2",
      // Test toolkits
      "org.apache.pekko" %% "pekko-multi-node-testkit" % pekkoVersion % Test,
      "org.apache.pekko" %% "pekko-actor-testkit-typed" % pekkoVersion % Test,
      // Logging
      "org.slf4j" % "slf4j-simple" % "2.0.17"
    )
  )

// Defines a new task activating assertion.
lazy val activateAssertion =
  TaskKey[Unit]("activate_assertions", "Activates assertions when running test.")
ThisBuild / activateAssertion := {
  println("Assertion activated.")
  OscarBuildParameters.enableAssertions = true
}
// Links the activateAssertion task to the task test.
Test / test := ((Test / test) dependsOn activateAssertion).value
