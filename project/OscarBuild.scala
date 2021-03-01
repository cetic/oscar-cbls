package oscar

import sbt.Keys._
import sbt._

object OscarBuild {

  lazy val PerfTest = config("perf") extend (Test)

  lazy val buildSettings = Seq(
    organization := "oscar",
    version := "4.1.0-SNAPSHOT",
    scalaVersion := "2.13.1",
    sbtVersion := "1.3.4"
  )

  lazy val commonSettings = buildSettings ++ Defaults.coreDefaultSettings ++ Seq(
    scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature",
    "-unchecked", "-language:implicitConversions",
    "-language:postfixOps"),
    licenses += ("LGPL-3.0", url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    testOptions in Test += ((target in Test) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    parallelExecution in Test := false,
    fork in Test := true,
    javaOptions in Test += "-Djava.library.path=../lib:../lib/",
    javacOptions ++= Seq("-encoding", "UTF-8"),
    unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "examples",
    publishTo := {
      val artifactoryName = "Artifactory Realm"
      val artifactoryUrl = "http://130.104.228.131/artifactory/"
      if (isSnapshot.value)
        Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
      else
        Some(artifactoryName at artifactoryUrl + "libs-release-local")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    testOptions in PerfTest += ((target in PerfTest) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    fork in PerfTest := true,
    parallelExecution in PerfTest := false,
  ) ++ (if(!OscarBuildParameters.debug) Seq(scalacOptions in Compile ++= Seq("-optimize", "-Xdisable-assertions", "-opt:l:inline", "-opt-inline-from:oscar.**")) else Seq())

  object Resolvers {
    val xypron = "Xypron Release" at "http://rsync.xypron.de/repository/"
    val leadoperations = "AWS S3 Release Repository" at "http://maven.leadoperations.co/release"
    val cogcomp = "Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/"
    val ingi = "INGI Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
  }

  object Dependencies {
    // Regular libraries
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "4.7.2"
    val jcommon = "org.jfree" % "jcommon" % "1.0.24"
    val jfreechart = "org.jfree" % "jfreechart" % "1.5.0"
    val jsci = "net.sf.jsci" % "jsci" % "1.2"
    val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
    val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "2.1.1"
    val swingx = "org.swinglabs" % "swingx" % "1.6.1"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "latest.milestone"
    //    val xcsp3 = "xcsp3"  % "xcsp3" % "1.0.0-SNAPSHOT"
    val xcsp3 = "org.xcsp" % "xcsp3-tools" % "1.1.0"
    val graphStreamCore = "org.graphstream" % "gs-core" % "1.3"
    val graphStreamAlgo = "org.graphstream" % "gs-algo" % "1.3"
    val graphStreamUI = "org.graphstream" % "gs-ui" % "1.3"
    val scallop = "org.rogach" % "scallop_2.11" % "1.0.0"

    // Akka
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.6.1"
    val akkaRemote = "com.typesafe.akka" %% "akka-remote" % "2.6.1"

    // Test libraries
    val junit = "junit" % "junit" % "4.12" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "3.1.0" % Test

    val junit2 = "junit" % "junit" % "4.12" % PerfTest
    val scalaCheck2 = "org.scalacheck" %% "scalacheck" % "1.14.0" % PerfTest
    val scalaTest2 = "org.scalatest" %% "scalatest" % "3.2.0-M2" % PerfTest

    val testDeps = Seq(junit, scalaCheck, scalaTest)
  }
}
