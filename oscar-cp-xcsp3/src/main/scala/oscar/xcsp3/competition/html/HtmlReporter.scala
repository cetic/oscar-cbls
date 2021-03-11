package oscar.xcsp3.competition.html

import java.io.File
import java.text.NumberFormat

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

/**
  * Utility class to generate an html report from a benchmark directory.
  */
object HtmlReporter extends App{
  if(args.length == 1) generateHtml(args(0))
  else println("Incorrect number of arguments!")

  /**
    * Generates an html report from the given xml object.
    */
  def generateHtml(directory: String): Unit = {
    val (timeout, instances, objTypes, solvers, configs, allSols, allStatus, solsByCommit, statusByCommit) = scanInstances(directory)



    instances.foreach(i => {
      if(objTypes(i) != "none") {
        var c = 0
        var objective = "unknown"
        while (objective == "unknown" && c < configs.length) {
          if (allSols.contains(i) && allSols(i).contains(configs(c))) {
            val sols = allSols(i)(configs(c))
            if (sols.length > 1) {
              if (sols.last._1 < sols.head._1) objective = "min"
              else objective = "max"
            }
          }
          c += 1
        }
        objTypes(i) = objective
      }
    })

    val allScores = mutable.Map[String, Int]()
    val scoresByCommit = mutable.Map[String, mutable.Map[String, Int]]()

    instances.foreach(i => {
      val bests = mutable.HashSet[String]()
      var bestTime = Long.MaxValue

      val objType = objTypes(i)
      if(objType == "none"){
        if (allStatus.contains(i)) allStatus(i).foreach { case (config, status) =>
          val (configStatus, configTime) = status
          if(configStatus == "SATISFIABLE" || configStatus == "UNSATISFIABLE") {
            if (configTime < bestTime) {
              bests.clear()
              bestTime = configTime
            }
            if(configTime == bestTime) bests += config
          }
        }
      }
      else if(objType != "unknown") {
        var bestVal = objType match {
          case "max" => Int.MinValue
          case "min" => Int.MaxValue
        }
        if (allSols.contains(i)) allSols(i).foreach { case (config, sols) =>
          if (sols.nonEmpty) {
            val (lastVal, lastTime) = sols.last
            if ((objType == "min" && lastVal < bestVal) || (objType == "max" && lastVal > bestVal)){
              bests.clear()
              bests += config
              bestVal = lastVal
              bestTime = lastTime
            }
            else if (lastVal == bestVal){
              if(lastTime < bestTime){
                bests.clear()
                bests += config
                bestTime = lastTime
              }
              else if(lastTime == bestTime) bests += config
            }
          }
        }
      }

      bests.foreach(config => {
        if (allScores.contains(config)) allScores(config) += 1
        else allScores += config -> 1
      })
    })

    solsByCommit.foreach{case (commit, sols) =>
      val scores = mutable.Map[String, Int]()

      instances.foreach(i => {
        val bests = mutable.HashSet[String]()
        var bestTime = Long.MaxValue
        val objType = objTypes(i)
        if (objType == "none") {
          val status = statusByCommit.getOrElse(commit, mutable.Map[String, mutable.Map[String, (String, Long)]]())

          if (status.contains(i)) status(i).foreach { case (config, confStatus) =>
            val (configStatus, configTime) = confStatus
            if(configStatus == "SATISFIABLE" || configStatus == "UNSATISFIABLE") {
              if (configTime < bestTime) {
                bests.clear()
                bestTime = configTime
              }
              if(configTime == bestTime) bests += config
            }
          }
        }
        else if(objType != "unknown") {
          var bestVal = objType match {
            case "max" => Int.MinValue
            case "min" => Int.MaxValue
          }

          if (sols.contains(i)) sols(i).foreach { case (solver, solValues) =>
            if (solValues.nonEmpty) {
              val (lastVal, lastTime) = solValues.last
              if ((objType == "min" && lastVal < bestVal) || (objType == "max" && lastVal > bestVal)) {
                bests.clear()
                bests += solver
                bestVal = lastVal
                bestTime = lastTime
              }
              else if (lastVal == bestVal){
                if(lastTime < bestTime){
                  bests.clear()
                  bests += solver
                  bestTime = lastTime
                }
                else if(lastTime == bestTime) bests += solver
              }
            }
          }
        }

        bests.foreach(solver => {
          if (scores.contains(solver)) scores(solver) += 1
          else scores += solver -> 1
        })
      })

      scoresByCommit += commit -> scores
    }

    val htmlWriter = new HtmlWriter("oscar-perf/src/main/scala/oscar/anytime/lns/utils/chart_report_template.html", directory + "/" + "htmlReport.html")
    htmlWriter.addHeading("Benchmark configuration")

    htmlWriter.addParagraph("Time allocated: " + timeout + " seconds")

    htmlWriter.addHeading("Solvers", 2)
    solvers.foreach(solver => {
      htmlWriter.addParagraph(solver)
    })

    htmlWriter.addHeading("Versions", 2)
    solsByCommit.keys.foreach(version => {
      htmlWriter.addParagraph(version)
    })

//    htmlWriter.addHeading("Instances", 2)
//    instances.foreach(instance => {
//      htmlWriter.addParagraph(instance)
//    })

    htmlWriter.addHeading("Scores")

    htmlWriter.addHeading("Score per solver on all versions", 2)
    htmlWriter.addElement(
      "table",
      "Score per solver on all versions",
      HtmlWriter.tableToHtmlString(Array(
        configs.map("'" + _ + "'").toArray,
        configs.map(config => allScores.getOrElse(config, 0).toString).toArray
      ))
    )

    htmlWriter.addHeading("Score per solver per version", 2)
    htmlWriter.addElement(
      "table",
      "Score per solver per version",
      HtmlWriter.tableToHtmlString(renderScores(solvers, scoresByCommit))
    )

    htmlWriter.addHeading("Results")
    solsByCommit.foreach{case (commit, sols) =>
      val commitStatus = statusByCommit.getOrElse(commit, mutable.Map[String, mutable.Map[String, (String, Long)]]())
      htmlWriter.addHeading(commit, 2)
      htmlWriter.addElement(
        "table",
        "Best solutions found",
        HtmlWriter.tableToHtmlString(renderBestSols(solvers, instances, objTypes, sols, commitStatus))
      )
    }

    htmlWriter.writeToHtml()
  }


  //Scans each config_instance file
  def scanInstances(directory: String): (
    Long,
    Seq[String],
    mutable.Map[String, String],
    Seq[String],
    Seq[String],
    mutable.Map[String, mutable.Map[String, Seq[(Int, Long)]]],
    mutable.Map[String, mutable.Map[String, (String, Long)]],
    mutable.Map[String, mutable.Map[String, mutable.Map[String, Seq[(Int, Long)]]]],
    mutable.Map[String, mutable.Map[String, mutable.Map[String, (String, Long)]]]
  ) = {

    val instances = mutable.HashSet[String]()
    val objTypes = mutable.Map[String, String]()
    val solvers = mutable.HashSet[String]()
    val configs = mutable.HashSet[String]()
    val allSols = mutable.Map[String, mutable.Map[String, Seq[(Int, Long)]]]()
    val allStatus = mutable.Map[String, mutable.Map[String, (String, Long)]]()
    val solsByCommit = mutable.Map[String, mutable.Map[String, mutable.Map[String, Seq[(Int, Long)]]]]()
    val statusByCommit = mutable.Map[String, mutable.Map[String, mutable.Map[String, (String, Long)]]]()

    val dirs = IOUtils.getFolderContent(directory).filter(d => d.isDirectory && !d.getName.startsWith("."))
    var maxTimeout = 0L

    dirs.foreach(dir => {
      val dirName = dir.getName

      val solsByInstance = mutable.Map[String, mutable.Map[String, Seq[(Int, Long)]]]()
      val statusByInstance = mutable.Map[String, mutable.Map[String, (String, Long)]]()

//      println("reading: " + file.getPath)
      val files = IOUtils.getFiles(dir.getAbsolutePath, ".txt")
      files.foreach(file =>{
        val (solver, timeout, instance, itype, sols, status) = readFile(file)

        if (timeout > maxTimeout) maxTimeout = timeout

        instances += instance
        solvers += solver
        val config = solver + "-" + dirName
        configs += config

        if(itype == "csp") objTypes += instance -> "none"
        else objTypes += instance -> "unknown"

        if (solsByInstance.contains(instance)) solsByInstance(instance) += solver -> sols
        else {
          val map = mutable.Map[String, Seq[(Int, Long)]]()
          map += solver -> sols
          solsByInstance += instance -> map
        }

        if (statusByInstance.contains(instance)) statusByInstance(instance) += solver -> status
        else {
          val map = mutable.Map[String, (String, Long)]()
          map += solver -> status
          statusByInstance += instance -> map
        }

        if (allSols.contains(instance)) allSols(instance) += config -> sols
        else {
          val map = mutable.Map[String, Seq[(Int, Long)]]()
          map += config -> sols
          allSols += instance -> map
        }

        if (allStatus.contains(instance)) allStatus(instance) += config -> status
        else {
          val map = mutable.Map[String, (String, Long)]()
          map += config -> status
          allStatus += instance -> map
        }
      })

      solsByCommit += dirName -> solsByInstance
      statusByCommit += dirName -> statusByInstance
    })

    (maxTimeout, instances.toSeq.sorted, objTypes, solvers.toSeq.sorted, configs.toSeq.sorted, allSols, allStatus, solsByCommit, statusByCommit)
  }

  def readFile(file: File): (String, Int, String, String, Seq[(Int, Long)], (String, Long)) = {

    var solver = ""
    var timeout = 240
    var instance = ""
    var itype = "unknown"
    val sols = ListBuffer[(Int, Long)]()
    var status = ("UNKNOWN", 0L)

    for (line <- Source.fromFile(file).getLines()) {
      val words = line.split(" ")
      try {
        val time = words(0).toLong
        words(1) match {
          case "c" => words(2) match {
            case "instance:" => instance = words(3).stripSuffix(".xml")
            case "solver:" => solver = words(3)
            case "timeout:" => timeout = words(3).toInt
            case "type:" => itype = words(3)
            case _ =>
          }

          case "o" => sols += ((words(2).toInt, time))

          case "s" => if(words(2) != "UNKNOWN") status = (words(2), time)

          case "d" => status = (words(2), time)

          case _ =>
        }
      }catch{
        case _:NumberFormatException =>
          println("Warning: Bad line!")
          if(line == "Missing Implementation") status = ("UNSUPPORTED", 0L)
      }
    }

    (solver, timeout, instance, itype, sols.toSeq, status)
  }

  def renderScores(solvers: Seq[String], scoresByCommits: mutable.Map[String, mutable.Map[String, Int]]): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Version'") ++ solvers.map("'" + _ + "'")
    scoresByCommits.foreach{case (commit, scores) =>
      array += Array("'" + commit + "'") ++ solvers.map(solver => scores.getOrElse(solver, 0).toString)
    }
    array.toArray
  }

  def renderBestSols(
                      solvers: Seq[String],
                      instances: Seq[String],
                      objTypes: mutable.Map[String, String],
                      sols: mutable.Map[String, mutable.Map[String, Seq[(Int, Long)]]],
                      status: mutable.Map[String, mutable.Map[String, (String, Long)]]
                    ): Array[Array[String]] = {
    val array = ArrayBuffer[Array[String]]()
    array += Array("'Instance'", "'Objective'") ++ solvers.map("'" + _ + "'")
    instances.foreach(instance => {
      if(sols.contains(instance))
        array += Array("'" + instance + "'", "'" + objTypes.getOrElse(instance, "unknown") + "'") ++ solvers.map(solver => {
          val solStatus = if(status.contains(instance) && status(instance).contains(solver)) status(instance)(solver) else ("UNKNOWN", 0L)
          if(sols(instance).contains(solver)){
            val solValues = sols(instance)(solver)
            if(solValues.nonEmpty){
              val lastValue = NumberFormat.getIntegerInstance().format(solValues.last._1)
              val lastTime = solValues.last._2/1000.0
              if(solStatus._1 == "OPTIMUM") "'" + lastValue + "* (" + lastTime + ", " + (status(instance)(solver)._2/1000.0) + ")'"
              else "'" + lastValue + " (" + lastTime + ")'"
            }
            else "'" + solStatus._1 + " (" + (solStatus._2/1000.0) + ")'"
          }
          else "'NO_DATA'"
        })
      else array += Array("'" + instance + "'", "'unknown'") ++ Array.fill[String](solvers.length)("'NO_DATA'")
    })
    array.toArray
  }
}
