ThisBuild / scalaVersion := "2.13.10"
ThisBuild / organization := "oscar"
ThisBuild / version      := "6.0.0-SNAPSHOT"

lazy val oscarCbls = (project in file("."))
  .enablePlugins(PackPlugin)
  .settings(
    licenses += ("LGPL-3.0", url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    Compile / scalacOptions ++= Seq(
      // Refer to https://docs.scala-lang.org/overviews/compiler-options/index.html for details
      "-deprecation",
      "-language:postfixOps" // not officially recommended, but often used in the project
//      "-Xdisable-assertions",
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
