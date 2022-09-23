import sbt.Keys._
import sbt._

object OscarBuild {

  lazy val PerfTest = config("perf") extend Test

  lazy val buildSettings = Seq(
    organization := "oscar",
    version := "5.0.0-SNAPSHOT",
    scalaVersion := "2.13.9",
    sbtVersion := "1.7.1"
  )

  lazy val commonSettings = buildSettings ++ Defaults.coreDefaultSettings ++ Seq(
    Compile / scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature",
    "-unchecked", "-language:implicitConversions", "-language:postfixOps", "-opt-warnings"),
    licenses += ("LGPL-3.0", url("https://www.gnu.org/licenses/lgpl-3.0.en.html")),
    Test / testOptions += ((Test / target) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    Test / parallelExecution := false,
    Test / fork := true,
    Test / javaOptions += "-Djava.library.path=../lib:../lib/",
    javacOptions ++= Seq("-encoding", "UTF-8"),
    Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "examples",
    publishTo := {
      val artifactoryName = "Artifactory Realm"
      val artifactoryUrl = "http://130.104.228.131/artifactory/"
      val artifactoryRepo = if (isSnapshot.value)
        Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
      else {
        Some(artifactoryName at artifactoryUrl + "libs-release-local")
      }
      artifactoryRepo.map(_.withAllowInsecureProtocol(true))
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    PerfTest / testOptions += ((PerfTest / target) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    PerfTest / fork := true,
    PerfTest / parallelExecution := false
  ) ++ (if (!OscarBuildParameters.debug) Seq(Compile / scalacOptions ++= Seq("-Xdisable-assertions", "-opt:l:inline", "-opt-inline-from:oscar.**"))
        else Seq()) ++ ceticSpecificSettings

  def ceticSpecificSettings = {
    if(Option(System.getProperty("cetic")).isDefined) Seq(
      publishTo := {
        val artifactoryName = "Artifactory Realm"
        val artifactoryUrl = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
        val artifactoryRepo = if (isSnapshot.value)
          Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
        else {
          Some(artifactoryName at artifactoryUrl + "libs-release-local")
        }
        artifactoryRepo.map(_.withAllowInsecureProtocol(true))
      }
    )
    else Seq()
  }

  object Resolvers {
    val xypron = ("Xypron Release" at "http://rsync.xypron.de/repository/").withAllowInsecureProtocol(true)
    val leadoperations = ("AWS S3 Release Repository" at "http://maven.leadoperations.co/release").withAllowInsecureProtocol(true)
    val cogcomp = ("Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/").withAllowInsecureProtocol(true)
    val ingi = ("INGI Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/").withAllowInsecureProtocol(true)
  }

  object Dependencies {
    // Regular libraries
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "4.11.1"
    val jcommon = "org.jfree" % "jcommon" % "1.0.24"
    val jfreechart = "org.jfree" % "jfreechart" % "1.5.3"
    val jsci = "net.sf.jsci" % "jsci" % "1.2"
    val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "2.1.0"
    val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    val scalaParallel = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    val swingx = "org.swinglabs" % "swingx" % "1.6.1"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "1.3.04"
    //    val xcsp3 = "xcsp3"  % "xcsp3" % "1.0.0-SNAPSHOT"
    val xcsp3 = "org.xcsp" % "xcsp3-tools" % "2.0.1"
    val graphStreamCore = "org.graphstream" % "gs-core" % "2.0"
    val graphStreamAlgo = "org.graphstream" % "gs-algo" % "2.0"
    val graphStreamUI = "org.graphstream" % "gs-ui" % "1.3"
    val scallop = "org.rogach" % "scallop_2.11" % "4.1.0"
    val jxmapviewer2 = "org.jxmapviewer" % "jxmapviewer2" % "2.6"
    val jtscore = "org.locationtech.jts" % "jts-core" % "1.19.0"
    val slf4j = "org.slf4j" % "slf4j-simple" % "2.0.0"

    // Akka
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.6.20"
    val akkaRemote = "com.typesafe.akka" %% "akka-remote" % "2.6.20"
    val akkaActorTyped = "com.typesafe.akka" %% "akka-actor-typed" % "2.6.20"
    val akkasl4j = "com.typesafe.akka" %% "akka-slf4j" % "2.6.20"

    // Test libraries
    val junit = "junit" % "junit" % "4.13.2" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "3.2.12" % Test
    val scalaTestPlus = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

    val junit2 = "junit" % "junit" % "4.13.2" % PerfTest
    val scalaCheck2 = "org.scalacheck" %% "scalacheck" % "1.17.0" % PerfTest
    val scalaTest2 = "org.scalatest" %% "scalatest" % "3.2.12" % PerfTest
    val scalaTestPlus2 = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % PerfTest

    val testDeps = Seq(junit, scalaCheck, scalaTest, scalaTestPlus)
  }
}
