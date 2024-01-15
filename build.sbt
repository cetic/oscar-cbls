ThisBuild / scalaVersion := "2.13.10"
ThisBuild / organization := "oscar"
ThisBuild / version      := "6.0.0-SNAPSHOT"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / publishTo    := {
  val artifactoryName = "Artifactory Realm"
  val artifactoryUrl = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
  val artifactoryRepo = if (isSnapshot.value)
    Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
  else {
    Some(artifactoryName at artifactoryUrl + "libs-release-local")
  }
  artifactoryRepo.map(_.withAllowInsecureProtocol(true))
}
ThisBuild / credentials += Credentials("Artifactory Realm", sys.env.getOrElse("ARTIFACTORY_URL", ""), sys.env.getOrElse("ARTIFACTORY_USER", ""), sys.env.getOrElse("ARTIFACTORY_PASS", ""))

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
//      "-Xdisable-assertions",
      "-opt-warnings:at-inline-failed-summary",
      "-opt:l:inline",
      "-opt-inline-from:oscar.**"
    ),
    name := "oscar-cbls"
  )
  .settings(PackPlugin.packSettings)
  .settings(packGenerateWindowsBatFile := false)
  .settings(
    libraryDependencies ++= Seq(
      "junit"              % "junit"           % "4.13.2"  % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.17.0"  % Test,
      "org.scalatest"     %% "scalatest"       % "3.2.15"  % Test,
      "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test
    )
  )
