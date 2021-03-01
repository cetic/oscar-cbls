import oscar.OscarBuild
import oscar.OscarBuild._

lazy val aggregatedProjects: Seq[ProjectReference] = Seq(oscarAlgebra, oscarAlgo, oscarCbls, oscarCp, oscarCPXcsp3, oscarPerf, oscarModeling, oscarDfo, oscarUtil, oscarVisual, oscarFzn, oscarFznCbls, oscarFznCp, oscarDes, oscarInvariants)

lazy val root = (project in file(".")) // has to be named root.
  .settings(commonSettings: _*)
  .aggregate(aggregatedProjects: _*)
  .settings(name := "oscar")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(ScalaUnidocPlugin.globalSettings)
  .settings(unidocProjectFilter in(ScalaUnidoc, unidoc) := inAnyProject -- inProjects(oscarFzn, oscarFznCbls, oscarFznCp, oscarPerf))
  .enablePlugins(PackPlugin)
  .settings(PackPlugin.packSettings)


lazy val oscarAlgebra = (project in file("oscar-algebra"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-algebra")
  .settings(libraryDependencies ++= Dependencies.testDeps)

lazy val oscarAlgo = (project in file("oscar-algo"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-algo")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .dependsOn(oscarUtil)
  .dependsOn(oscarVisual)


lazy val oscarCbls = (project in file("oscar-cbls")) // TODO pack : pack auto settings?
  .settings(commonSettings: _*)
  .settings(name := "oscar-cbls")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.scalaSwing)
  .enablePlugins(PackPlugin)
  .settings(PackPlugin.packSettings)
  .settings(packGenerateWindowsBatFile := false)
  .dependsOn(oscarVisual)


lazy val oscarCp = (project in file("oscar-cp")) // TODO pack : pack auto settings?
  .settings(commonSettings: _*)
  .settings(name := "oscar-cp")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.scalaParserCombinators)
  .enablePlugins(PackPlugin)
  .settings(PackPlugin.packSettings)
  .dependsOn(oscarAlgo)
  .dependsOn(oscarVisual)

lazy val oscarCpExample = (project in file("oscar-cp-examples")) // TODO pack : pack auto settings?
  .settings(commonSettings: _*)
  .settings(name := "oscar-cp-examples")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.scalaParserCombinators)
  .enablePlugins(PackPlugin)
  .settings(PackPlugin.packSettings)
  .dependsOn(oscarCp)

lazy val oscarModeling = (project in file("oscar-modeling"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-modeling")
  .settings(scalacOptions in Compile ++= Seq("-language:reflectiveCalls"))
  .settings(resolvers ++= Seq(OscarBuild.Resolvers.xypron))
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.graphStreamCore
    :+ Dependencies.graphStreamAlgo :+ Dependencies.graphStreamUI :+ Dependencies.scallop
    :+ Dependencies.akkaActor :+ Dependencies.scalaSwing :+ Dependencies.jfreechart :+ Dependencies.jcommon)
  .dependsOn(oscarCp)

lazy val oscarCPXcsp3 = (project in file("oscar-cp-xcsp3"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-cp-xcsp3")
  .settings(scalacOptions in Compile ++= Seq("-language:reflectiveCalls"))
  .settings(resolvers ++= Seq(OscarBuild.Resolvers.xypron))
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.xcsp3)
  .dependsOn(oscarCp)
  .dependsOn(oscarModeling)


lazy val oscarMl = (project in file("oscar-ml")) // TODO pack : pack auto settings?
  .settings(commonSettings: _*)
  .settings(name := "oscar-ml")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.scalaParserCombinators)
  .enablePlugins(PackPlugin)
  .settings(PackPlugin.packSettings)
  .dependsOn(oscarCp)


lazy val oscarPerf = (project in file("oscar-perf")) // TODO check configs PerfTest?
  .settings(commonSettings: _*)
  .settings(name := "oscar-perf")
  .settings(scalacOptions in Compile ++= Seq("-language:reflectiveCalls"))
  .settings(resolvers ++= Seq(OscarBuild.Resolvers.ingi))
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.xcsp3)
  .dependsOn(oscarCp)
  .dependsOn(oscarCPXcsp3)
  .dependsOn(oscarModeling)
  .configs(PerfTest)
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .settings(inConfig(PerfTest)(Defaults.testTasks ++ Seq()): _*)
  .settings(inConfig(PerfTest)(baseDirectory in PerfTest := file(".")))
  .settings(testOptions in Test := Seq(Tests.Filter(x => !(x endsWith "PerfTest"))))
  .settings(testOptions in PerfTest := Seq(Tests.Filter(_ endsWith "PerfTest")))


// Not included in the default build
lazy val oscarDes = (project in file("oscar-des"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-des")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.jsci)
  .dependsOn(oscarInvariants)


lazy val oscarDfo = (project in file("oscar-dfo"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-dfo")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.jcommon :+ Dependencies.jfreechart)
  .dependsOn(oscarAlgebra)
  .dependsOn(oscarAlgo)
  .dependsOn(oscarVisual)


// Not included in the default build
lazy val oscarFzn = (project in file("oscar-fzn"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-fzn")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.antlr4Runtime)
lazy val oscarFznCbls = (project in file("oscar-fzn-cbls"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-fzn-cbls")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .dependsOn(oscarCbls)
  .dependsOn(oscarFzn)
  .dependsOn(oscarFznCp)

lazy val oscarFznCp = (project in file("oscar-fzn-cp"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-fzn-cp")
  .settings(libraryDependencies ++= Dependencies.testDeps)
  .dependsOn(oscarCp)
  .dependsOn(oscarFzn)

// Not included in the build
lazy val oscarInvariants = (project in file("oscar-invariants"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-invariants")
  .settings(libraryDependencies ++= Dependencies.testDeps)

lazy val oscarUtil = (project in file("oscar-util"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-util")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.scalaXml)

lazy val oscarVisual = (project in file("oscar-visual"))
  .settings(commonSettings: _*)
  .settings(name := "oscar-visual")
  .settings(libraryDependencies ++= Dependencies.testDeps :+ Dependencies.jfreechart :+ Dependencies.swingx :+ Dependencies.swingxWs :+ Dependencies.scalaXml)
  .dependsOn(oscarUtil)



