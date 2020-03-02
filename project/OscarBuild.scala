package oscar

import sbt._
import sbt.Keys._
import de.johoop.jacoco4sbt.JacocoPlugin._
import xerial.sbt.Pack._
import sbtunidoc.Plugin._
import java.util.Date

object OscarBuild extends Build {

  lazy val PerfTest = config("perf") extend Test

  object BuildSettings {
    val buildOrganization = "oscar"
    val buildVersion = "4.1.0-CBLS-GEOM-SNAPSHOT"
    val buildScalaVersion = "2.12.8"
    val buildSbtVersion= "0.13.18"

    lazy val commonSettings = Defaults.defaultSettings ++  jacoco.settings ++ Seq(
      organization := buildOrganization,
      version := buildVersion,
      scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature",
        "-unchecked", "-Xdisable-assertions", "-language:implicitConversions",
        "-language:postfixOps"),
      scalacOptions in Test := Seq("-optimise"),
      testOptions in Test <+= (target in Test) map {
        t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports") ) },
      parallelExecution in Test := false,
      fork in Test := true,
      javaOptions in Test += "-Djava.library.path=../lib:../lib/",
      javacOptions ++= Seq("-encoding", "UTF-8"),
      scalaVersion := buildScalaVersion,
      unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "examples",
      publishTo := {
        val artifactoryName = "Artifactory Realm"
        val artifactoryUrl = "http://130.104.230.89/artifactory/"
        if (isSnapshot.value)
          Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
        else
          Some(artifactoryName at artifactoryUrl + "libs-release-local")
      },
      credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),

      testOptions in PerfTest <+= (target in PerfTest) map {
        t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports") ) },
      fork in PerfTest := true,
      parallelExecution in PerfTest := false

    ) ++ ceticSpecificSettings

  }

  object Resolvers {
    val xypron = "Xypron Release" at "http://rsync.xypron.de/repository/"
    val leadoperations = "AWS S3 Release Repository" at "http://maven.leadoperations.co/release"
    val cogcomp = "Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/"
    val ingi = "INGI Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
    val mvnrepository = "Maven Repository" at "https://mvnrepository.com/artifact/"
  }

  object Dependencies {
    // Regular libraries
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "latest.milestone"
    val jcommon = "org.jfree" % "jcommon" % "latest.milestone"
    val jfreechart = "org.jfree" % "jfreechart" % "latest.milestone"
    val jsci = "net.sf.jsci" % "jsci" % "latest.milestone"
    val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "latest.milestone"
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "latest.milestone"
    val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "latest.milestone"
    val swingx = "org.swinglabs" % "swingx" % "1.0"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "latest.milestone"
    val xcsp3 = "xcsp3"  % "xcsp3" % "1.0.0-SNAPSHOT"
    val graphStreamCore = "org.graphstream" % "gs-core" % "1.3"
    val graphStreamAlgo = "org.graphstream" % "gs-algo" % "1.3"
    val graphStreamUI = "org.graphstream" % "gs-ui" % "1.3"
    val scallop = "org.rogach" % "scallop_2.11" % "1.0.0"
    val jxmapviewer2 = "org.jxmapviewer" % "jxmapviewer2" % "2.2"
    val jtscore = "org.locationtech.jts" % "jts-core" % "1.16.0"

    // Akka
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.5.6"
    val akkaRemote = "com.typesafe.akka" %% "akka-remote" % "2.5.6"

    // Test libraries
    val junit = "junit" % "junit" % "latest.milestone" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.+" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "3.0.4" % Test

    val junit2 = "junit" % "junit" % "latest.milestone" % PerfTest
    val scalaCheck2 = "org.scalacheck" %% "scalacheck" % "1.13.+" % PerfTest
    val scalaTest2 = "org.scalatest" %% "scalatest" % "3.0.4" % PerfTest

    val testDeps = Seq(junit, scalaCheck, scalaTest)
  }

  import BuildSettings._
  import Dependencies._
  import Resolvers._
  import UnidocKeys._

  def ceticSpecificSettings = {
    if(Option(System.getProperty("cetic")).isDefined) Seq(
      publishTo := {
        val artifactory = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
        if (isSnapshot.value)
          Some("Artifactory Realm" at artifactory + "libs-snapshot;build.timestamp=" + new java.util.Date().getTime)
        else
          Some("Artifactory Realm" at artifactory + "libs-release")
      },
      packageOptions += Package.ManifestAttributes(
        ("REVISION_ID", System.getProperty("REVISION_ID")),
        ("REVISION_URL", "https://bitbucket.org/oscarlib/oscar/commits/"+System.getProperty("REVISION_ID") ),
        ("JENKINS_BUILD_ID", System.getProperty("BUILD_ID")),
        ("BUILD_DATE", new Date().toString)
      )
    )
    else Seq()
  }

  lazy val oscar = Project(
    id = "oscar",
    base = file("."),
    settings =
      commonSettings ++
        packSettings ++
        unidocSettings ++
        Seq(libraryDependencies ++= testDeps) :+
        (unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(oscarFzn, oscarFznCbls, oscarFznCp, oscarPerf)),
    aggregate = Seq(oscarAlgebra, oscarAlgo, oscarCbls, oscarCp, oscarCPXcsp3, oscarPerf, oscarModeling, oscarDfo, oscarUtil, oscarVisual, oscarFzn, oscarFznCbls, oscarFznCp, oscarDes, oscarInvariants)
  )

  lazy val oscarAlgebra = Project(
    id = "oscar-algebra",
    base = file("oscar-algebra"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps)
  )

  lazy val oscarAlgo = Project(
    id = "oscar-algo",
    base = file("oscar-algo"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps),
    dependencies = Seq(oscarUtil, oscarVisual)
  )

  lazy val oscarCbls = Project(
    id = "oscar-cbls",
    base = file("oscar-cbls"),
    settings =
      commonSettings ++
        packAutoSettings ++
        Seq(
          resolvers ++= Seq(mvnrepository),
          libraryDependencies ++= testDeps :+ scalaSwing :+ jxmapviewer2 :+ jtscore,
          packGenerateWindowsBatFile := false
        ),
    dependencies = Seq(oscarVisual)
  )

  lazy val oscarCp = Project(
    id = "oscar-cp",
    base = file("oscar-cp"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ scalaParserCombinators),
    dependencies = Seq(oscarAlgo, oscarVisual)
  )

  lazy val oscarModeling = Project(
    id = "oscar-modeling",
    base = file("oscar-modeling"),
    settings =
      commonSettings ++
        Seq(
          scalacOptions in Compile ++= Seq("-language:reflectiveCalls"),
          resolvers ++= Seq(xypron),
          libraryDependencies ++= testDeps :+ graphStreamCore :+ graphStreamAlgo :+ graphStreamUI :+ scallop
            :+ akkaActor :+ scalaSwing :+ jfreechart :+ jcommon),
    dependencies = Seq(oscarCp)
  )

  lazy val oscarCPXcsp3 = Project(
    id = "oscar-cp-xcsp3",
    base = file("oscar-cp-xcsp3"),
    settings =
      commonSettings ++
        Seq(
          resolvers ++= Seq(ingi),
          libraryDependencies ++= testDeps :+ xcsp3),
    dependencies = Seq(oscarCp, oscarModeling)
  )

  lazy val oscarPerf = Project(
    id = "oscar-perf",
    base = file("oscar-perf"),
    settings = commonSettings ++ Seq(resolvers ++= Seq(ingi), libraryDependencies ++= testDeps :+ xcsp3),
    dependencies = Seq(oscarCp, oscarCPXcsp3, oscarModeling)
  ).configs( PerfTest )
    .settings(libraryDependencies ++= testDeps)
    .settings(inConfig(PerfTest)(Defaults.testTasks ++ Seq()): _*)
    .settings(inConfig(PerfTest)(baseDirectory in PerfTest := file(".")))
    .settings(
      testOptions in Test := Seq(Tests.Filter(x => !(x endsWith "PerfTest"))),
      testOptions in PerfTest := Seq(Tests.Filter(_ endsWith "PerfTest"))
    )


  // Not included in the root build
  lazy val oscarDes = Project(
    id = "oscar-des",
    base = file("oscar-des"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ jsci),
    dependencies = Seq(oscarInvariants)
  )

  lazy val oscarDfo = Project(
    id = "oscar-dfo",
    base = file("oscar-dfo"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ jcommon :+ jfreechart),
    dependencies = Seq(oscarAlgebra, oscarAlgo, oscarVisual)
  )

  // Not included in the default build
  lazy val oscarFzn = Project(
    id = "oscar-fzn",
    base = file("oscar-fzn"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ antlr4Runtime)
  )

  lazy val oscarFznCbls = Project(
    id = "oscar-fzn-cbls",
    base = file("oscar-fzn-cbls"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps),
    dependencies = Seq(oscarCbls,oscarFzn,oscarFznCp)
  )

  lazy val oscarFznCp = Project(
    id = "oscar-fzn-cp",
    base = file("oscar-fzn-cp"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps),
    dependencies = Seq(oscarCp,oscarFzn)
  )


  // Not included in the build
  lazy val oscarInvariants = Project(
    id = "oscar-invariants",
    base = file("oscar-invariants"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps)
  )

  lazy val oscarUtil = Project(
    id = "oscar-util",
    base = file("oscar-util"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ scalaXml)
  )

  lazy val oscarVisual = Project(
    id = "oscar-visual",
    base = file("oscar-visual"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ jfreechart :+ swingx :+ swingxWs),
    dependencies = Seq(oscarUtil)
  )
}
