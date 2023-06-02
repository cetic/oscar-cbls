ThisBuild / scalaVersion := "2.13.10"
ThisBuild / organization := "oscar"
ThisBuild / version      := "6.0.0-SNAPSHOT"


lazy val oscarCbls = (project in file(".")) // TODO pack : pack auto settings?
  .enablePlugins(PackPlugin)
  .settings(
    licenses += ("LGPL-3.0", url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    Compile / scalacOptions ++= Seq(
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-language:postfixOps" // Allow postfix operator notation, such as 1 to 10 toList (not recommended)
    ),
    name := "oscar-cbls"
  )
  .settings(PackPlugin.packSettings)
  .settings(packGenerateWindowsBatFile := false)
  .settings(libraryDependencies ++= Seq(
    "junit"              % "junit"           % "4.13.2"  % Test,
    "org.scalacheck"    %% "scalacheck"      % "1.17.0"  % Test,
    "org.scalatest"     %% "scalatest"       % "3.2.14"  % Test,
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test
  ))
