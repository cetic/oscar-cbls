ThisBuild / scalaVersion  := "2.13.14"
ThisBuild / organization  := "oscar"
ThisBuild / version       := sys.props.getOrElse("version", "latest")
ThisBuild / versionScheme := Some("early-semver")

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
      "com.gluonhq"        % "maps"            % "2.0.0-ea+6",
      "org.scalafx"       %% "scalafx"         % "22.0.0-R33",
      "junit"              % "junit"           % "4.13.2"  % Test,
      "org.scalacheck"    %% "scalacheck"      % "1.18.0"  % Test,
      "org.scalatest"     %% "scalatest"       % "3.2.19"  % Test,
      "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test
    )
  )

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
