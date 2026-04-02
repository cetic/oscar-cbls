ThisBuild / scalaVersion  := "2.13.17"
ThisBuild / organization  := "oscar"
ThisBuild / version       := sys.props.getOrElse("version", "latest")
ThisBuild / versionScheme := Some("early-semver")

val pekkoVersion = "1.4.0"

lazy val oscarCbls = (project in file("."))
  .enablePlugins(PackPlugin)
  .settings(
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
    name := "oscar-cbls"
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

////////////////////////////////////////
// This part is used only for CETIC's internal CI/CD. The user can ignore it.

ThisBuild / publishTo := {
  val nexus = "https://nexus.cetic.be/"
  val privateRepo  = "repository/oscar"
  val publicRepo = "repository/oscar-public"

  val isTag = sys.env.contains("CI_COMMIT_TAG")

  val repo = if (isTag) publicRepo else privateRepo
  Some("Nexus" at nexus + repo)
}

ThisBuild / credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "nexus.cetic.be",
  sys.env.getOrElse("NEXUS_USER", ""),
  sys.env.getOrElse("NEXUS_PASS", "")
)

