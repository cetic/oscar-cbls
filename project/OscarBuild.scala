import sbt.Keys._
import sbt._

object OscarBuild {

  lazy val PerfTest = config("perf") extend Test

  lazy val buildSettings = Seq(
    organization := "oscar",
    version := "5.0.0-SNAPSHOT",
    scalaVersion := "2.13.8",
    sbtVersion := "1.6.2"
  )

  lazy val commonSettings = buildSettings ++ Defaults.coreDefaultSettings ++ Seq(
    scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature",
    "-unchecked", "-language:implicitConversions", "-language:postfixOps", "-opt-warnings"),
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
      val artifactoryRepo = if (isSnapshot.value)
        Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
      else {
        Some(artifactoryName at artifactoryUrl + "libs-release-local")
      }
      artifactoryRepo.map(_.withAllowInsecureProtocol(true))
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    testOptions in PerfTest += ((target in PerfTest) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    fork in PerfTest := true,
    parallelExecution in PerfTest := false
  ) ++ (if (!OscarBuildParameters.debug) Seq(scalacOptions in Compile ++= Seq("-Xdisable-assertions", "-opt:l:inline", "-opt-inline-from:oscar.**"))
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
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "4.9.3"
    val jcommon = "org.jfree" % "jcommon" % "1.0.24"
    val jfreechart = "org.jfree" % "jfreechart" % "1.5.3"
    val jsci = "net.sf.jsci" % "jsci" % "1.2"
    val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "2.0.1"
    val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
    val scalaParallel = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    val swingx = "org.swinglabs" % "swingx" % "1.6.1"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "1.3.04"
    //    val xcsp3 = "xcsp3"  % "xcsp3" % "1.0.0-SNAPSHOT"
    val xcsp3 = "org.xcsp" % "xcsp3-tools" % "1.2.4"
    val graphStreamCore = "org.graphstream" % "gs-core" % "2.0"
    val graphStreamAlgo = "org.graphstream" % "gs-algo" % "2.0"
    val graphStreamUI = "org.graphstream" % "gs-ui" % "1.3"
    val scallop = "org.rogach" % "scallop_2.11" % "4.1.0"
    val jxmapviewer2 = "org.jxmapviewer" % "jxmapviewer2" % "2.6"
    val jtscore = "org.locationtech.jts" % "jts-core" % "1.18.2"
    val slf4j = "org.slf4j" % "slf4j-simple" % "1.7.36"

    // Akka
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.6.18"
    val akkaRemote = "com.typesafe.akka" %% "akka-remote" % "2.6.18"
    val akkaActorTyped = "com.typesafe.akka" %% "akka-actor-typed" % "2.6.18"
    val akkasl4j = "com.typesafe.akka" %% "akka-slf4j" % "2.6.18"

    // Test libraries
    val junit = "junit" % "junit" % "4.13.2" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9" % Test
    val scalaTestPlus = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test

    val junit2 = "junit" % "junit" % "4.13.2" % PerfTest
    val scalaCheck2 = "org.scalacheck" %% "scalacheck" % "1.15.4" % PerfTest
    val scalaTest2 = "org.scalatest" %% "scalatest" % "3.2.9" % PerfTest
    val scalaTestPlus2 = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % PerfTest

    val testDeps = Seq(junit, scalaCheck, scalaTest, scalaTestPlus)
  }
}
