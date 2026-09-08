ThisBuild / scalaVersion := "2.13.17"
ThisBuild / organization := "be.cetic"
ThisBuild / version := {
  sys.props.get("version") match {
    case Some(v) =>
      // If version name is defined as an environment variable (namely during gitlab ci)
      v
    case None if sys.env.contains("SONATYPE_USERNAME") =>
      // If version is not defined as an environment variable, but we are pushing on sonatype (namely during github ci)
      (ThisBuild / version).value
    case None =>
      // If none of is above is true (namely for local developments)
      "latest"
  }
}
ThisBuild / versionScheme := Some("early-semver")

val pekkoVersion = "1.7.0"

// Maven central metadata
ThisBuild / homepage := Some(url("https://github.com/cetic/oscar-cbls"))
ThisBuild / licenses := List("LGPL-3.0" -> url("https://www.gnu.org/licenses/lgpl-3.0.en.html"))
ThisBuild / developers := List(
  Developer(
    id = "cetic",
    name = "CETIC",
    email = "info@cetic.be",
    url = url("https://www.cetic.be")
  )
)
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/cetic/oscar-cbls"), "scm:git@github.com:cetic/oscar-cbls.git")
)
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"

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
      "com.gluonhq"  % "maps"    % "2.0.0-ea+6",
      "org.scalafx" %% "scalafx" % "22.0.0-R33",
      // Test dependencies
      "junit"              % "junit"           % "4.13.2"  % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.19.0",
      "org.scalatest"     %% "scalatest"       % "3.2.20",
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
      "io.altoo" %% "pekko-kryo-serialization" % "1.5.2",
      // Test toolkits
      "org.apache.pekko" %% "pekko-multi-node-testkit"  % pekkoVersion % Test,
      "org.apache.pekko" %% "pekko-actor-testkit-typed" % pekkoVersion % Test,
      // Logging
      "org.slf4j" % "slf4j-simple" % "2.0.18",
      // Command line parsing, used by the runnable examples
      "com.github.scopt" %% "scopt" % "4.1.0"
    )
  )
  .settings(
    ///////////////////////////////////////////////////////////////////////////
    // Standalone "fat jar" of the multi-JVM examples.
    //
    // Built on demand with `sbt assembly`; the resulting jar is NOT part of the
    // artifacts produced by `package` / `publish`, so the library publication is
    // left untouched. Run it with:
    //   java -jar target/scala-2.13/oscar-cbls-examples-<version>.jar --help
    ///////////////////////////////////////////////////////////////////////////
    assembly / mainClass       := Some("oscar.cbls.examples.MultiJVMExampleRunner"),
    assembly / assemblyJarName := s"oscar-cbls-examples-${version.value}.jar",
    assembly / assemblyMergeStrategy := {
      // Pekko splits its configuration across one reference.conf per module: they must all be
      // kept, otherwise the actor system fails to start from the fat jar.
      case PathList(ps @ _*) if ps.last == "reference.conf"   => MergeStrategy.concat
      case PathList(ps @ _*) if ps.last == "application.conf" => MergeStrategy.concat
      case PathList(ps @ _*) if ps.last == "version.conf"     => MergeStrategy.concat
      // Same for the ServiceLoader registrations.
      case PathList("META-INF", "services", _*) => MergeStrategy.filterDistinctLines
      // JPMS descriptors are meaningless in a fat jar, and signatures of the original jars
      // become invalid once their content is merged.
      case PathList(ps @ _*) if ps.last == "module-info.class" => MergeStrategy.discard
      case PathList("META-INF", ps @ _*)
          if ps.lastOption
            .exists(p => p.endsWith(".SF") || p.endsWith(".DSA") || p.endsWith(".RSA")) =>
        MergeStrategy.discard
      case PathList("META-INF", _*) => MergeStrategy.discard
      // Several dependencies ship the same helper classes/resources; keeping the first one is
      // enough here and avoids a deduplicate error on every new transitive dependency.
      case _ => MergeStrategy.first
    }
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

publishTo := Def.taskIf {
  val isPublishingToSonatype = sys.env.contains("SONATYPE_USERNAME")

  if (isPublishingToSonatype) {
    publishTo.value
  } else {
    val nexus       = "https://nexus.cetic.be/"
    val privateRepo = "repository/oscar"
    val publicRepo  = "repository/oscar-public"

    val isTag = sys.env.contains("CI_COMMIT_TAG")

    val repo = if (isTag) publicRepo else privateRepo
    Some("Nexus" at nexus + repo)
  }
}.value

ThisBuild / credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "nexus.cetic.be",
  sys.env.getOrElse("NEXUS_USER", ""),
  sys.env.getOrElse("NEXUS_PASS", "")
)
